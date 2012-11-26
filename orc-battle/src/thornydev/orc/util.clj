(ns thornydev.orc.util)

;; macro from Chas Emerick 2010 blog post to have default values
;; specified in a defrecord definition
;; not sure if there is a more "standard" way to do it yet
;; source: http://cemerick.com/2010/08/02/defrecord-slot-defaults/

(defmacro defrecord+defaults
  "Defines a new record, along with a new-RecordName factory function that
   returns an instance of the record initialized with the default values
   provided as part of the record's slot declarations.  e.g.
   (defrecord+ Foo [a 5 b \"hi\"])
   (new-Foo)
   => #user.Foo{:a 5, :b \"hi\"}"
  [name slots & etc]
  (let [fields (->> slots (partition 2) (map first) vec)
        defaults (->> slots (partition 2) (map second))]
    `(do
       (defrecord ~name
         ~fields
         ~@etc)
       (defn ~(symbol (str "new-" name))
         ~(str "A factory function returning a new instance of " name
               " initialized with the defaults specified in the corresponding defrecord+ form.")
         []
         (~(symbol (str name \.)) ~@defaults))
       ~name)))


(defn randval
  "Generates a random positive integer from 1 .. n inclusive"
  [n]
  (inc (rand-int (max 1 n))))
