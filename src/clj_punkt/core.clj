(ns clj-punkt.core
  (:require [clojure.string :as str]
            [clojure.edn :as edn])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:import [java.lang Math]
           [java.lang Character]
           [java.text Normalizer Normalizer$Form]))

(def include-all-collocs false)
(def include-abbrev-collocs false)
(def unaccent-types false)

;; Exclude some chars from starting words
(def re-word-start #"[^\(\"\`{\[:;&\#\*@\)}\]\-,]")
;; Characters that cannot appear within words
(def re-non-word-chars #"(?:[?!)\";}\]\*:@\'\({\[ ])")
;; Hyphen and ellipsis are multi-character punctuation
(def re-multi-char-punct #"(?:\-{2,}|\.{2,}|(?:\.\s){2,}\.)")
(def re-numeric #"^-?[\.,]?\d[\d,\.-]*\.?$")
;; Matches token types that are not merely punctuation.
(def re-non-punct #"[^\W\d]")
(def re-ellipsis #"^\.\.+$")

(def re-initial #"^[^\W\d]\.$")
(def re-alpha #"[^\W\d]+$")
(def re-non-punct #"[^\W\d]")

(def re-sent-end-chars #"[\.?!…]")
(def internal-punctuations #{\, \: \;})
(def punctuations #{\; \: \, \. \! \?})

(def re-url #"(?:\w{3,5}://\S+)|(?:www\.\S+)")
(def re-domain #"(\.fr|\.com|\.net|\.org|\.uk|\.de|\.it|\.es)\.?$")

(defrecord Trainer
    [type-fdist
     collocation-fdist
     sent-starter-fdist
     abbrev-types
     num-period-toks
     ortho-context
     sentbreak-count
     sent-starters
     collocations])

(defn make-new-trainer
  [& [loading]]
  (let [model (merge
               {:type-fdist (atom {})
                :collocation-fdist (atom {})
                :sent-starter-fdist (atom {})
                :abbrev-types (atom #{})
                :num-period-toks (atom 0)
                :ortho-context (atom {})
                :sentbreak-count (atom 0)
                :sent-starters (atom #{})
                :collocations (atom #{})}
               (or loading {}))]
    (Trainer.
     (:type-fdist model)
     (:collocation-fdist model)
     (:sent-starter-fdist model)
     (:abbrev-types model)
     (:num-period-toks model)
     (:ortho-context model)
     (:sentbreak-count model)
     (:sent-starters model)
     (:collocations model))))

(defn load-model
  [path]
  (make-new-trainer
   (reduce (fn [acc [k v]]
             (assoc acc k (atom v)))
           {}
           (edn/read-string (slurp path)))))

(defn store-model
  [train path]
  (binding [*print-length* nil]
    (spit path (pr-str (reduce (fn [acc [k v]]
                                 (assoc acc k (deref v)))
                               {}
                               train)))))

;; cut-off value whether a 'token' is an abbreviation
(def abbrev 0.3)
(def abbrev-backoff 5)
;; this sets a minimum bound on the number of times a bigram needs to
;; appear before it can be considered a collocation, in addition to log
;; likelihood statistics. This is useful when INCLUDE_ALL_COLLOCS is True.
(def min-colloc-freq 1)
;; minimal log-likelihood value that two tokens need to be considered
;; as a collocation
(def collocation 7.88)
;; minimal log-likelihood value that two tokens need to be considered
;; as a collocation
(def sent-starter 30)

;; The following constants are used to describe the orthographic
;; contexts in which a word can occur.
(def ortho-beg-uc (bit-shift-left 1 1))
(def ortho-mid-uc (bit-shift-left 1 2))
(def ortho-unk-uc (bit-shift-left 1 3))
(def ortho-beg-lc (bit-shift-left 1 4))
(def ortho-mid-lc (bit-shift-left 1 5))
(def ortho-unk-lc (bit-shift-left 1 6))
(def ortho-uc (+ ortho-beg-uc ortho-mid-uc ortho-unk-uc))
(def ortho-lc (+ ortho-beg-lc ortho-mid-lc ortho-unk-lc))

(def ortho-map
  {[:initial  :upper] ortho-beg-uc
   [:internal :upper] ortho-mid-uc
   [:unknown  :upper] ortho-unk-uc
   [:initial  :lower] ortho-beg-lc
   [:internal :lower] ortho-mid-lc
   [:unknown  :lower] ortho-unk-lc})

(defn belongs-to-context?
  [context case]
  (if (> (bit-and context case) 0)
    true
    false))

(defn- inv
  [n]
  (- 0 n))

(defn get-str
  [tok]
  (if (map? tok) (:type tok) tok))

(defn replace-number
  [^String string]
  (let [rep (str/replace string re-numeric "##number##")
        period (re-find #"\.$" string)]
    (if (and period (not= string rep)) (str rep ".") rep)))

(defn unaccent-text
  [^String string]
  (str/replace #"\p{InCombiningDiacriticalMarks}+" ""
                  (Normalizer/normalize string Normalizer$Form/NFD)))
(defn get-type
  "Returns a case and diacritics-normalized representation of the token."
  [tok]
  (try+
   (if (nil? tok)
     ""
     (let [type (get-str tok)
           clean-type (if unaccent-types (unaccent-text type) type)]
       (-> clean-type
          (str/lower-case)
          (replace-number))))
   (catch Object _
     (throw+ (str "error getting type of token " (str "[" tok "]"))
             (:throwable &throw-context)))))

(defn is-ellipsis?
  [tok]
  (re-find re-ellipsis (get-type tok)))

(defn is-period-final?
  [tok]
  (and (> (count (get-type tok)) 1)
       (= (last (get-type tok)) \.)))

(defn strip-final-period
  [tok]
  (let [typ (get-type tok)]
    (if (is-period-final? tok)
      (str/replace typ #"\.$" "")
      typ)))

(defn get-type-no-period
  "The type with its final period removed if it has one."
  [tok]
  (let [type (get-type tok)]
    (strip-final-period type)))

(defn get-type-no-sentperiod
  "The type with its final period removed if it is marked as a sentence break."
  [tok]
  (if (:sentbreak tok)
    (let [type (get-type tok)]
      (get-type-no-period type))
    (get-type tok)))

(defn is-number?
  [tok]
  (= (get-type-no-period tok) "##number##"))

(defn is-initial?
  [tok]
  (re-find re-initial (get-type tok)))

(defn is-non-punct?
  [tok]
  (re-find re-non-punct (get-type tok)))

(defn first-upper?
  [tok]
  (let [s (first (get-str tok))]
    (and s (Character/isUpperCase s))))

(defn is-alpha?
  [tok]
  (re-find re-alpha (get-type tok)))

(defn first-lower?
  [tok]
  (let [s (first (get-str tok))]
    (and s (Character/isLowerCase s))))

(defn get-first-case
  [tok]
  (cond
   (first-upper? tok) :upper
   (first-lower? tok) :lower
   :else :none))

(defn pair-tokens
  [tokens]
  (partition 2 1 (concat tokens [nil])))

;; Regular expression to split punctuation from words, excluding period.
(def tokenizer-pattern
  (re-pattern
   (str
    re-url
     "|"
     re-multi-char-punct
     "|"
     "(?=" re-word-start ")\\S+?" ;; Accept word characters until end is found
     "(?="
       "\\s|"                                        ;; White-space
       "$|"                                          ;; End-of-string
       re-non-word-chars "|" re-multi-char-punct "|" ;; Punctuation
       "," "(?=$|\\s|" re-non-word-chars "|" re-multi-char-punct ")" ;; Comma if at end of word
     ")"
     "|\\S")))

(def period-context-pattern
  (re-pattern
   (str
    "\\S*"
    re-sent-end-chars
    "(?=(" re-non-word-chars "|"
    "\\s+(\\S+)))")))

(defn split-chunk
  [re text]
  (let [m (re-matcher re text)
        end-text (count text)]
    (loop [chunks   []
           last-pos 0]
      (if (.find m)
        (let [start (.start m)
              end (.end m)
              frag (.group m)]
          (recur (conj chunks {:start last-pos :end start :fragment (subs text last-pos start)}) end))
        (conj chunks {:start last-pos :end end-text
                      :fragment (subs text last-pos end-text)})))))

(defn tokenize-words
  "Divide the given text into tokens, using the punkt word
segmentation regular expression, and generate the resulting list
of tokens augmented as maps with two boolean values for whether
the given token occurs at the start of a paragraph or a new line,
respectively."
  [plaintext]
  (flatten
   (let [parastart (atom true)
         matcher (re-matcher tokenizer-pattern plaintext)]
     (for [{l-start :start l-end :end l-frag :fragment :as line} (split-chunk #"\n|\r\n" plaintext)]
       (let [trim-line (str/trim l-frag)]
         (if (> (count trim-line) 0)
           (do
             (doto matcher
               (.region l-start l-end)
               (.useTransparentBounds true))
             (let [toks (loop [matches []
                               linestart true]
                          (if (and (.find matcher) (.group matcher))
                            (do
                              (recur
                               (conj matches {:start (.start matcher)
                                              :end (.end matcher)
                                              :type (.group matcher)
                                              :linestart linestart
                                              :parastart (let [par @parastart]
                                                           (swap! parastart (fn [_] false))
                                                           par)})
                               false))
                            matches))]
               toks))
           (do
             (swap! parastart (fn [_] true))
             [])))))))

(defn dunning-log-likelihood
  "A function that calculates the modified Dunning log-likelihood
   ratio scores for abbreviation candidates."
  [count-a count-b count-ab N]
  (let [p1 (/ (float count-b) N)
        p2 0.99
        null-hypo (+ (* (float count-ab) (Math/log p1))
                     (* (- count-a count-ab)
                        (Math/log (- 1.0 p1))))
        alt-hypo (+ (* (float count-ab) (Math/log p2))
                     (* (- count-a count-ab)
                        (Math/log (- 1.0 p2))))
        likelihood (- null-hypo alt-hypo)]
    (* -2.0 likelihood)))

(defn col-log-likelihood
  [count-a count-b count-ab N]
  (let [p (/ count-b (float N))
        p1 (/ count-ab (float count-a))
        p2 (/ (- count-b count-ab) (float (- N count-a)))
        summand1 (+ (* count-ab (Math/log p))
                    (* (- count-a count-ab) (Math/log (- 1.0 p))))
        summand2 (+ (* (- count-b count-ab) (Math/log p))
                    (* (+ (- N count-a count-b) count-ab)
                       (Math/log (- 1.0 p))))
        summand3 (if (= count-a count-ab)
                   0
                   (+ (* count-ab (Math/log p1))
                      (* (- count-a count-ab) (Math/log (- 1.0 p1)))))
        summand4 (if (= count-b count-ab)
                   0
                   (+ (* (- count-b count-ab) (Math/log p2))
                      (* (+ (- N count-a count-b) count-ab) (Math/log (- 1.0 p2)))))
        likelihood (- (+ summand1 summand2) summand3 summand4)
        score (* -2.0 likelihood)]
        score))

(defn find-tokens-freqs
  [tokens]
  (let [freqs (reduce (fn [acc tok]
                        (let [type (get-type tok)]
                          (update-in acc [type]
                                     (fn [old] (if (nil? old) 1 (inc old))))))
                      {} tokens)
        num-periods (reduce (fn [c [type freq]]
                              (if (is-period-final? type)
                                (+ c freq)
                                c))
                            0 freqs)]
    [freqs num-periods]))

(defn filter-candidate-abbrev
  "Check some basic conditions, to rule out words that are clearly not abbrev types."
  [known-abbrev type]
  (not (or (is-number? type)
           (re-find re-domain (get-type type))
           (re-find re-url (get-type type))
           (not (re-find re-non-punct type))
           (if (is-period-final? type)
             (contains? known-abbrev type)
             (not (contains? known-abbrev type))))))

(defn reclassify-abbrev-types
  [types train]
  (let [known-abbrev (deref (:abbrev-types train))
        type-fdist (deref (:type-fdist train))
        num-period-toks (deref (:num-period-toks train))
        candidates (filter #(filter-candidate-abbrev known-abbrev %) types)
        N (reduce + 0 (vals type-fdist))]
    (for [type candidates]
      (let [type-without-period (strip-final-period type)
            add (not= type type-without-period)
            num-periods (inc (count (filter #(= % \.) type-without-period)))
            num-non-periods (inc (- (count type-without-period) num-periods))
            type-with-period (str type-without-period ".")
            count-with-period (get type-fdist type-with-period 0)
            count-without-period (get type-fdist type-without-period 0)
            ll (dunning-log-likelihood
                (+ count-with-period count-without-period)
                num-period-toks
                count-with-period
                N)
            f-length (Math/exp (inv num-non-periods))
            f-periods num-periods
            f-penalty (Math/pow num-non-periods (inv count-without-period))
            score (* ll f-length f-periods f-penalty)]
        [type-without-period score add]))))

(defn rare-abbrev-type?
  "A word type is counted as a rare abbreviation if...
      - it's not already marked as an abbreviation
      - it occurs fewer than ABBREV_BACKOFF times
      - either it is followed by a sentence-internal punctuation
        mark, *or* it is followed by a lower-case word that
        sometimes appears with upper case, but never occurs with
        lower case at the beginning of sentences."
  [train cur-tok next-tok]
  (if (or (:abbr cur-tok) (not (:sentbreak cur-tok)))
    false
    ;; Find the case-normalized type of the token.  If it's
    ;; a sentence-final token, strip off the period.
    (let [typ (get-type-no-sentperiod cur-tok)
          type-fdist (deref (:type-fdist train))
          count (+
                 (if (is-period-final? cur-tok)
                   (get type-fdist (get-type-no-period cur-tok) 0)
                   0)
                 (get type-fdist (get-type cur-tok) 0))]
      ;; Proceed only if the type hasn't been categorized as an
      ;; abbreviation already, and is sufficiently rare...
      (cond
       (or (contains? (deref (:abbrev-types train)) typ) (>= count abbrev-backoff)) false
       ;; urls are not abbrev
       (re-find re-domain typ) false
       (re-find re-url typ) false
       ;; Record this token as an abbreviation if the next
       ;; token is a sentence-internal punctuation mark.
       (contains? internal-punctuations (first (get-type next-tok))) true
       ;; Record this type as an abbreviation if the next
       ;; token...  (i) starts with a lower case letter,
       ;; (ii) sometimes occurs with an uppercase letter,
       ;; and (iii) never occus with an uppercase letter
       ;; sentence-internally.
       (first-lower? next-tok)
       (let [typ2 (get-type-no-sentperiod next-tok)
             typ2-ortho (get (deref (:ortho-context train)) typ2 0)]
         (if (and
              (belongs-to-context? typ2-ortho ortho-beg-uc)
              (not (belongs-to-context? typ2-ortho ortho-mid-uc)))
           true
           false))
       :else false))))

(defn potential-sent-starter?
  "Uses collocation heuristics for each candidate token to
   determine if it frequently starts sentences."
  [cur-tok prev-tok]
  ;; If a token (i) is preceeded by a sentence break that is
  ;; not a potential ordinal number or initial, and (ii) is
  ;; alphabetic, then it is a a sentence-starter.
  (and (:sentbreak prev-tok)
       (not (or (is-number? prev-tok) (is-initial? prev-tok)))
       (is-alpha? cur-tok)))

(defn find-sent-starters
  "Uses collocation heuristics for each candidate token to
   determine if it frequently starts sentences."
  [train]
  (filter
   #(identity %)
   (for [[typ typ-at-break-count] (deref (:sent-starter-fdist train))
         :when typ]
     (let [type-fdist (deref (:type-fdist train))
           typ-count (+ (get type-fdist typ 0) (get type-fdist (str typ ".") 0))]
       ;; needed after freq_threshold
       (if-not (< typ-count typ-at-break-count)
         (let [N (reduce + 0 (vals type-fdist))
               sentbreak-count (deref (:sentbreak-count train))
               ll (col-log-likelihood sentbreak-count
                                      typ-count
                                      typ-at-break-count
                                      N)]
           (if (and (>= ll sent-starter)
                    (> (/ (float N) sentbreak-count)
                       (/ typ-count typ-at-break-count)))
             [typ ll])))))))

(defn potential-collocation?
  [tok1 tok2]
  (and (or include-all-collocs
           (and include-abbrev-collocs (:abbr tok1))
           (and (:sentbreak tok1)
                (or (is-initial? tok1) (is-number? tok1))))
       (is-non-punct? tok1)
       (is-non-punct? tok2)
       (not (:parastart tok2))))

(defn find-collocations
  "Generates likely collocations and their log-likelihood."
  [train]
  (filter
   #(identity %)
   (for [[[typ1 typ2] col-count] (deref (:collocation-fdist train))
         :when (or (and typ1 typ2)
                   (contains? (deref (:sent-starters train)) typ2))]
     (let [type-fdist (deref (:type-fdist train))
           typ1-count (+ (get type-fdist typ1 0) (get type-fdist (str typ1 ".") 0))
           typ2-count (+ (get type-fdist typ2 0) (get type-fdist (str typ2 ".") 0))]
       (if (and (> typ1-count 1)
                (> typ2-count 1)
                (< min-colloc-freq col-count)
                (<= col-count (min typ1-count typ2-count)))
         (let [N (float (reduce + 0 (vals type-fdist)))
               ll (col-log-likelihood typ1-count typ2-count col-count N)]
           (if (and (> ll collocation)
                    (> (/ N typ1-count) (/ typ2-count col-count)))
             [[typ1 typ2] ll])))))))

(defn add-ortho-context!
  [train typ flag]
  (swap! (:ortho-context train)
         (fn [old typ flag]
           (assoc old typ (bit-or (get old typ 0) flag)))
         typ flag))

(defn get-orthography-data!
  [train tokens]
  (let [context (atom :internal)]
    (doseq [tok tokens]
      ;; If we encounter a paragraph break, then it's a good sign
      ;; that it's a sentence break.  But err on the side of
      ;; caution (by not positing a sentence break) if we just
      ;; saw an abbreviation.
      (if (and (:parastart tok) (not= @context :unknown))
        (swap! context (fn [_] :initial)))
      ;; If we're at the beginning of a line, then err on the
      ;; side of calling our context 'initial'.
      (if (and (:linestart tok) (= @context :internal))
        (swap! context (fn [_] :unknown)))
      ;; Find the case-normalized type of the token.  If it's a
      ;; sentence-final token, strip off the period.
      (let [typ (get-type-no-period tok)
            ;; Update the orthographic context table.
            flag (get ortho-map [@context (get-first-case tok)] 0)]
        (if (> flag 0)
          (add-ortho-context! train typ flag))
        ;; Decide whether the next word is at a sentence boundary.
        (cond (:sentbreak tok)
              (if-not (or (is-number? tok) (is-initial? tok))
                (swap! context (fn [_] :initial))
                (swap! context (fn [_] :unknown)))
              (or (:ellipsis tok) (:abbr tok))
              (swap! context (fn [_] :unknown))
              :else
              (swap! context (fn [_] :internal)))))))

(defn annotate-first-pass
  [train token]
  (merge
   token
   (cond
    (re-find re-sent-end-chars (get-type token)) {:sentbreak true}
    (is-ellipsis? token) {:ellipsis true}
    (is-period-final? token) (if (contains? (deref (:abbrev-types train)) (strip-final-period token))
                            {:abbr true}
                            {:sentbreak true})
    :else {})))

(defn ortho-heuristic?
  "Decide whether the given token is the first token in a sentence."
  [train tok]
  (if (contains? punctuations (first (get-type tok)))
    false
    (let [ortho-context (get (deref (:ortho-context train)) (get-type-no-sentperiod tok) 0)]
      (cond
       ;; If the word is capitalized, occurs at least once with a
       ;; lower case first letter, and never occurs with an upper case
       ;; first letter sentence-internally, then it's a sentence
       ;; starter.
       (and (first-upper? tok)
            ortho-context
            (bit-and ortho-context ortho-lc)
            (not (belongs-to-context? ortho-context ortho-mid-uc))) true
            ;; If the word is lower case, and either (a) we've seen it used
            ;; with upper case, or (b) we've never seen it used
            ;; sentence-initially with lower case, then it's not a sentence
            ;; starter.
            (and (first-lower? tok)
                 (or (belongs-to-context? ortho-context ortho-uc)
                     (not (belongs-to-context? ortho-context ortho-beg-lc))))
            false
            :else
            nil))))

(defn annotate-second-pass
  "Performs token-based classification over a pair of contiguous tokens
   returning an updated augmented token for the first of them."
  [train [tok1 tok2]]
  ;; If next token is a paragraph beginning, label tok as
  ;; a sentence break.
  (if (:parastart tok2)
    (assoc tok1 :sentbreak true)
    (if-not (and tok2 (is-period-final? tok1))
      tok1
      (let [typ (get-type-no-period tok1)
            next-tok (get-type tok2)
            next-typ (get-type-no-sentperiod tok2)
            is-initial (is-initial? tok1)
            is-abbr-or-ellipsis (and (or (:abbr tok1) (:ellipsis tok1))
                                     (not is-initial))
            is-initial-or-number (or is-initial (is-number? tok1))
            ortho-next (ortho-heuristic? train tok2)
            ortho-next-context (get (deref (:ortho-context train)) next-typ 0)]
        (cond
         ;; [4.1.2. Collocational Heuristic] If there's a
         ;; collocation between the word before and after the
         ;; period, then label tok as an abbreviation and NOT
         ;; a sentence break. Note that collocations with
         ;; frequent sentence starters as their second word are
         ;; excluded in training.
         (contains? (deref (:collocations train)) [typ next-typ])
         (assoc tok1 :sentbreak false :abbr true)
         ;; [4.2. Token-Based Reclassification of Abbreviations] If
         ;; the token is an abbreviation or an ellipsis, then decide
         ;; whether we should *also* classify it as a sentbreak.

         ;; [4.1.1. Orthographic Heuristic] Check if there's
         ;; orthogrpahic evidence about whether the next word
         ;; starts a sentence or not.
         (and is-abbr-or-ellipsis ortho-next)
         (assoc tok1 :sentbreak true)
         ;; [4.1.3. Frequent Sentence Starter Heuristic] If the
         ;; next word is capitalized, and is a member of the
         ;; frequent-sentence-starters list, then label tok as a
         ;; sentence break.
         (and
          (or is-abbr-or-ellipsis is-initial-or-number)
          (first-upper? tok2)
          (contains? (deref (:sent-starters train)) next-typ))
         (assoc tok1 :sentbreak true)
         ;; [4.3. Token-Based Detection of Initials and Ordinals]
         ;; Check if any initials or ordinals tokens that are marked
         ;; as sentbreaks should be reclassified as abbreviations.

         ;; [4.1.1. Orthographic Heuristic] Check if there's
         ;; orthogrpahic evidence about whether the next word
         ;; starts a sentence or not.
         (and is-initial-or-number (not ortho-next))
         (assoc tok1 :sentbreak false :abbr true)
         ;; Special heuristic for initials: if orthogrpahic
         ;; heuristc is unknown, and next word is always
         ;; capitalized, then mark as abbrev (eg: J. Bach).
         (and (nil? ortho-next) is-initial (first-upper? tok2)
              (not (belongs-to-context? ortho-next-context ortho-lc)))
         (assoc tok1 :sentbreak true :abbr false)
         :else tok1)))))

(defn modify-atomic-set!
  [set fun new]
  (if (> (count new) 0)
    (swap! set
           (fn [old to-mod]
             (apply fun old to-mod))
           new)))

(defn- pairs->hash
  [kvs]
  (persistent!
   (reduce (fn [acc [k v]] (assoc! acc k v))
           (transient {})
           kvs)))

(defn- cut-freqs-below-thresh
  [freqs thresh]
  (pairs->hash (filter (fn [[k freq]]
                         (> freq thresh)) freqs)))

(defn cut-keys-below-freqs
  [coll thresh freqs]
  (persistent!
   (reduce (fn [new-coll [k freq]]
             (if-let [val (get coll k)]
               (if (> freq thresh)
                 (assoc! new-coll k val)
                 new-coll)
               new-coll))
           (transient {}) freqs)))

(defn train-tokens!
  [train toks]
  (let [[freqs num-periods] (find-tokens-freqs toks)
        unique-types (into #{} (keys freqs))]
    (swap! (:num-period-toks train) (fn [old c] (+ old c)) num-periods)
    (swap! (:type-fdist train) (fn [old new]
                          (merge-with + old new)) freqs)
    ;; Look for new abbreviations, and for types that no longer are
    (let [classif (reclassify-abbrev-types unique-types train)
          [add remove] (reduce (fn [[add remove] [type score to-add?]]
                                   (cond
                                    (and (>= score abbrev) to-add?) [(conj add type) remove]
                                    (and (< score abbrev) (not to-add?)) [add (conj remove type)]
                                    :else [add remove]))
                               [#{} #{}]
                               classif)]
      (dosync
       (modify-atomic-set! (:abbrev-types train) conj add)
       (modify-atomic-set! (:abbrev-types train) disj remove))
      ;; Make a preliminary pass through the document, marking likely
      ;; sentence breaks, abbreviations, and ellipsis tokens.
      (let [tokens (map #((partial annotate-first-pass train) %) toks)
            ;; We need total number of sentence breaks to find sentence starters
            sentbreak-count (reduce (fn [accu {:keys [sentbreak]}]
                                      (if sentbreak (inc accu) accu))
                                    0 tokens)]
        ;; Check what contexts each word type can appear in, given the
        ;; case of its first letter.
        (get-orthography-data! train tokens)
        (swap! (:sentbreak-count train) + sentbreak-count)
        ;; The remaining heuristics relate to pairs of tokens where the first
        ;; ends in a period.
        (doseq [[tok1 tok2] (pair-tokens tokens)
                :when (or (not (is-period-final? tok1)) (not (nil? tok2)))]
          ;; Is the first token a rare abbreviation?
          ;; (if (rare-abbrev-type? train tok1 tok2) (println tok1 tok2))
          ;; (if (rare-abbrev-type? train tok1 tok2)
          ;;   (swap! (:abbrev-types train) (fn [old tok]
          ;;                           (conj old (get-type-no-period tok))) tok1))
          ;; Does second token have a high likelihood of starting a sentence?
          (if (potential-sent-starter? tok2 tok1)
            (swap! (:sent-starter-fdist train) (fn [dist tok]
                                          (update-in dist [tok]
                                                     (fn [old] (if old (inc old) 1))))
                   (get-type-no-period tok2)))
          ;; Is this bigram a potential collocation?
          (if (potential-collocation? tok1 tok2)
            (swap! (:collocation-fdist train)
                   (fn [freqs tok1 tok2]
                                         (update-in freqs
                                                    [[(get-type-no-period tok1)
                                                      (get-type-no-sentperiod tok2)]]
                                                    (fn [old] (if old (inc old) 1))))
                   tok1 tok2)))))
    train))

(defn filter-rare-freqs!
  [train & {:keys [ortho-thresh type-thresh colloc-thresh sent-start-thresh]
            :or {ortho-thresh 2 type-thresh 2 colloc-thresh 2 sent-start-thresh 2}}]
  (doseq [[key thresh] [[:type-fdist type-thresh]
                        [:collocation-fdist colloc-thresh]
                        [:sent-starter-fdist sent-start-thresh]]]
    (swap! (key train) (fn [old thresh]
                         (cut-freqs-below-thresh old thresh)) thresh))
  (swap! (:ortho-context train) (fn [old thresh freqs]
                                  (cut-keys-below-freqs old ortho-thresh freqs))
         ortho-thresh
         (deref (:type-fdist train)))
  train)

(defn train!
  [train text & {:keys [clean]}]
  (train-tokens! train (tokenize-words text))
  (if clean
    (filter-rare-freqs! train)
    train))

(defn finalize-trainer!
  [train & {:keys [clean?] :or {clean? true}}]
  (reset! (:sent-starters train) (reduce
                                   (fn [starters [typ ll]]
                                     (conj starters typ))
                                   #{} (find-sent-starters train)))
  (reset! (:collocations train) (reduce
                                 (fn [collocs [typ ll]]
                                   (conj collocs typ))
                                 #{} (find-collocations train)))
  (if clean?
    (doseq [key [:type-fdist
                 :collocation-fdist
                 :sent-starter-fdist]]
      (reset! (key train) {})))
  train)

(defn annotate-tokens
  [train tokens]
  (map
   #(annotate-second-pass train %)
   (pair-tokens
    (map #(annotate-first-pass train %) tokens))))

(defn tag-sentbreak-tokens
  [train text]
  (let [tokens (tokenize-words text)
        full-tokens (annotate-tokens train tokens)]
    full-tokens))

(defn- emit-sentence
  [tokens]
  (let [start (:start (first tokens))
        end (:end (last tokens))]
    {:start start :end end :tokens tokens}))

(defn- accumulate-sentences
  [tokens]
  (loop [todo tokens
         [words sentences :as acc] [(transient []) (transient [])]]
    (if-let [{:keys [sentbreak] :as token} (first todo)]
      (if sentbreak
        (recur (rest todo) [(transient []) (conj! sentences (emit-sentence (persistent! (conj! words token))))])
        (recur (rest todo) [(conj! words token) sentences]))
      (persistent!
       (if (= 0 (count words))
         sentences
         (conj! sentences (emit-sentence (persistent! words))))))))

(defn extract-tokens
  [train text]
  (let [tokens (tag-sentbreak-tokens train text)
        chunks (accumulate-sentences tokens)
        sentences (map #(assoc % :text (subs text (:start %) (:end %))) chunks)]
    sentences))

(defn extract-sentences
  [train text]
  (let [chunks (extract-tokens train text)]
    (map :text chunks)))
