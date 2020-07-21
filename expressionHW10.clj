(defn constant [value] (fn [m] value))
(defn variable [varName] (fn [m] (get m varName)))
(defn abstractOperation [f] (fn [& args] (fn [m] (apply f (mapv (fn [x] (x m)) args)))))
(def negate (abstractOperation -))
(def multiply (abstractOperation *))
(def add (abstractOperation +))
(def exp (abstractOperation #(Math/exp %)))
(def ln (abstractOperation #(Math/log (Math/abs %))))
(def subtract (abstractOperation -))
(def divide (abstractOperation #(/ (double %1) (double %2))))

(def op {
                   "+"      add
                   "-"      subtract
                   "/"      divide
                   "*"      multiply
                   "negate" negate
                   "exp"    exp
                   "ln"     ln
                   })

(def variables
  {'x (variable "x")
   'y (variable "y")
   'z (variable "z")})

(defn parse [expression] (cond (number? expression) (constant expression)
                               (contains? variables expression) (variables expression)
                               :else (apply (get op (name (first expression))) (mapv parse (pop expression)))))

(defn parseFunction [expression]
      (parse (read-string expression)))