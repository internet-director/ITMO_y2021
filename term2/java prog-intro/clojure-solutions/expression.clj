(defn constant [v] (constantly v))
(defn variable [v] (fn[num] (num v)))
(defn base [o] (fn [& lr] (fn [args] (apply o (map (fn [x] (x args)) lr)))))
(def add (base +))
(def subtract (base -))
(def multiply (base *))
(def divide (base (fn [& arg] (cond 
	(not= (count arg) 1) (reduce (fn[a b](/ (double a) (double b))) arg)
	::else (/ 1.0 (double ( nth arg 0)))))))
(def negate (base -))
(def exp (base (fn[x] (Math/exp x))))
(def ln (base (fn[x] (Math/log x))))
(def operMap {'+ add, '- subtract, '* multiply, '/ divide, 'negate negate, 'exp exp, 'ln ln})

(defn parseFunction[expr] (cond
	(string? expr) (parseFunction(read-string expr))
	(symbol? expr) (variable (str expr))
	(number? expr) (constant expr)
	::else (apply (operMap(nth expr 0)) (map parseFunction(rest expr)))))
	

(defn proto-get [obj key]
	(cond
	(contains? obj key) (get obj key)
	(contains? obj :prototype) (recur (get obj :prototype) key)
	:else nil))
	
(defn proto-call [obj key & args]
	(apply (proto-get obj key) obj args))
	
(defn field [key] 
	(fn [obj] (proto-get obj key)))
	
(defn method [key] 
	(fn [obj & args] (apply proto-call obj key args)))
	
(def toString (method :toString))
(def evaluate (method :evaluate))
(def diff (method :diff))
(def arg (field :arg))

	
(defn Constant [value] {
	:toString (fn [this] (format "%.1f" value))
	:diff (fn [& args] (Constant 0.0))
	:evaluate (fn [this _] (:value this))
	:value value})
	
(def ZERO (Constant 0.0))
(def ONE  (Constant 1.0))

(defn Variable [value] {
	:toString (fn [this] (:value this))
	:diff (fn [this args]
                (cond
				(= args value) ONE
				:else ZERO))
	:evaluate (fn [this id](id (:value this)))
	:value value})


(defn base [oper action diff] (fn [& arg] {
	:toString (fn [this]  (str "(" oper " " (clojure.string/join " " (mapv toString (:arg this)))")"))
    :oper oper
    :action   action
    :diff diff
	:evaluate (fn [this vars]
                 (apply action (mapv (fn [o] (evaluate o vars)) arg )))
    :arg (vec arg)}))

(def Add (base '+ + (fn [this args] (Add (diff ((arg this) 0) args) (diff ((arg this) 1) args)))))
(def Subtract (base '- - (fn [this args] (Subtract (diff ((arg this) 0) args) (diff ((arg this) 1) args)))))
(def Multiply (base '* * (fn [this args] (Add (Multiply ((arg this) 1) (diff ((arg this) 0) args))
                                             (Multiply ((arg this) 0) (diff ((arg this) 1) args))))))
(def Divide (base '/ (fn [x y] (/ (double x) (double y)))
                    (fn [this args] (Divide (Subtract (Multiply ((arg this) 1) (diff ((arg this) 0) args))
                                                     (Multiply ((arg this) 0) (diff ((arg this) 1) args)))
                                           (Multiply ((arg this) 1) ((arg this) 1))))))
(def Negate (base 'negate - (fn [this args] (Negate (diff ((arg this) 0) args)))))
(def Exp (base 'exp (fn [x] (Math/exp x))
	(fn [this args] (Multiply (Exp ((arg this) 0)) (diff ((arg this) 0) args)))))	
(def Ln (base 'ln (fn [x] (Math/log x))
    (fn [this args] (Multiply (Divide (Constant 1.0) ((arg this) 0)) (diff ((arg this) 0) args)))))
					
(def newOperMap {'+ Add, '- Subtract, '* Multiply, '/ Divide, 'negate Negate, 'exp Exp, 'ln Ln})
  
(defn parseObject[expr] (cond
	(string? expr) (parseObject(read-string expr))
	(symbol? expr) (Variable (str expr))
	(number? expr) (Constant expr)
	::else (apply (newOperMap(nth expr 0)) (map parseObject(rest expr)))))