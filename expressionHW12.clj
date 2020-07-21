(def div #(/ (double %1) %2))
(def exp #(Math/exp %))
(def ln #(Math/log (Math/abs %)))

(defn proto-get [obj key]
      (cond
        (contains? obj key) (obj key)
        (contains? obj :prototype) (proto-get (obj :prototype) key)
        :else nil))

(defn proto-call [this key & args]
      (apply (proto-get this key) this args))

(defn field [key]
      (fn [this] (proto-get this key)))

(defn method [key]
      (fn [this & args] (apply proto-call this key args)))

(def evaluate (method :evaluate))
(def toString (method :toString))
(def toStringSuffix (method :toStringSuffix))
(def diff (method :diff))

(defn Constant [value]
      {
       :toString       (fn [_] (format "%.1f" value))
       :toStringSuffix (fn [_] (format "%.1f" value))
       :evaluate       (fn [_ _] value)
       :diff           (fn [_ _] (Constant 0))
       })

(def zero (Constant 0))
(def one (Constant 1))

(def var-proto
  (let [name (field :name)]
       {
        :toString       (fn [this] (name this))
        :toStringSuffix (fn [this] (name this))
        :evaluate       (fn [this keymap] (keymap (clojure.string/lower-case (first (name this)))))
        :diff           (fn [this var] (if (= var (name this)) one zero))
        }))

(defn Variable [name]
      {
       :prototype var-proto
       :name      name
       })

(def operation-proto
  (let [diffFunc (field :diffFunc)
        name (field :name)
        op (field :op)
        operands (field :operands)]
       {
        :toString       (fn [this]
                            (str "(" (name this) " " (clojure.string/join " " (map toString (operands this))) ")"))

        :toStringSuffix (fn [this]
                            (str "(" (clojure.string/join " " (map toStringSuffix (operands this))) " " (name this) ")"))

        :evaluate       (fn [this values]
                            (apply (op this) (map #(evaluate % values) (operands this))))

        :diff           (fn [this var] (let [curArgs (operands this) a (first curArgs) b (last curArgs)]
                                            (if (= (count curArgs) 2)
                                              ((diffFunc this) a b (diff a var) (diff b var))
                                              ((diffFunc this) a (diff a var)))))
        }))

(defn operation-params [op name diffFunc]
      {
       :prototype operation-proto
       :op        op
       :name      name
       :diffFunc  diffFunc
       })

(defn operation [op name diffFunc]
      (let [params-proto (operation-params op name diffFunc)]
           (fn [& operands]
               {
                :prototype params-proto
                :operands  (vec operands)
                })))

(def Add (operation + "+" (fn [_ _ da db] (Add da db))))

(def Subtract (operation - "-" (fn [_ _ da db] (Subtract da db))))

(def Multiply (operation * "*" (fn [a b da db] (Add (Multiply da b) (Multiply db a)))))

(def Divide (operation
              div "/" (fn [a b da db] (Divide (Subtract (Multiply b da)
                                                        (Multiply a db))
                                              (Multiply b b)))))

(def Negate (operation - "negate" (fn [_ da] (Subtract da))))

(def Exp (operation exp "exp" (fn [a da] (Multiply da (Exp a)))))

(def Ln (operation ln "ln" (fn [a da] (Multiply da (Divide (Constant 1) a)))))

(def ops {
          '+      Add,
          '-      Subtract,
          '*      Multiply,
          '/      Divide,
          'negate Negate,
          'exp    Exp,
          'ln     Ln
          })

(def variables
  {'x (Variable "x")
   'y (Variable "y")
   'z (Variable "z")})

(defn parse [expr] (cond (number? expr) (Constant expr)
                         (contains? variables expr) (variables expr)
                         (list? expr) (apply (ops (first expr)) (map parse (rest expr)))))

(defn parseObject [expression]
      (parse (read-string expression)))

(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)

(defn _empty [value] (partial -return value))

(defn _char [p]
      (fn [[c & cs]]
          (if (and c (p c)) (-return c cs))))

(defn _map [f]
      (fn [result]
          (if (-valid? result)
            (-return (f (-value result)) (-tail result)))))

(defn _combine [f a b]
      (fn [str]
          (let [ar ((force a) str)]
               (if (-valid? ar)
                 ((_map (partial f (-value ar)))
                  ((force b) (-tail ar)))))))

(defn _either [a b]
      (fn [str]
          (let [ar ((force a) str)]
               (if (-valid? ar) ar ((force b) str)))))

(defn _parser [p]
      (fn [input]
          (-value ((_combine (fn [v _] v) p (_char #{\u0000})) (str input \u0000)))))


(defn +char [chars] (_char (set chars)))

(defn +char-not [chars] (_char (comp not (set chars))))

(defn +map [f parser] (comp (_map f) parser))

(def +parser _parser)

(def +ignore (partial +map (constantly 'ignore)))

(defn iconj [coll value]
      (if (= value 'ignore) coll (conj coll value)))

(defn +seq [& ps]
      (reduce (partial _combine iconj) (_empty []) ps))

(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))

(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))

(defn +or [p & ps]
      (reduce (partial _either) p ps))

(defn +opt [p]
      (+or p (_empty nil)))

(defn +star [p]
      (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))

(defn +plus [p] (+seqf cons p (+star p)))

(defn +str [p] (+map (partial apply str) p))

(def *all-chars (mapv char (range 0 128)))
(def *space (+char (apply str (filter #(Character/isWhitespace (char %)) *all-chars))))
(def *letter (+char (apply str (filter #(Character/isLetter (char %)) *all-chars))))
(def *digit (+char (apply str (filter #(Character/isDigit (char %)) *all-chars))))
(def *ws (+ignore (+star *space)))

(def *constant (+map (comp Constant read-string)
                     (+str (+seq (+opt (+char "-")) (+str (+plus *digit)) (+char ".") (+str (+plus *digit))))))

(def *operations (+char "+-*/negate"))
(def *variables (+char "XYZxyz"))
(def *identifier (+str (+plus (+or *variables *operations))))
(def *function-or-variable (+map (comp #(ops % (Variable (str %))) symbol) *identifier))

(declare *value)
(defn *seq [begin p end] (+seqn 1 (+char begin) (+plus (+seqn 0 *ws p)) *ws (+char end)))
(def *list (+map (fn [list] (apply (last list) (butlast list))) (*seq "(" (delay *value) ")")))
(def *value (+or *constant *function-or-variable *list))

(def parseObjectSuffix (+parser (+seqn 0 *ws *value *ws)))