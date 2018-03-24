(ns pc-test.core
  (:require [no.disassemble :as nd]
            [clj-java-decompiler.core :refer [decompile]]
            [proteus :refer [let-mutable]]
            [primitive-math :as pmath]
            [insn.core :as insn]
            
            [insn.clojure :as bc])
  (:import [BBox]
           [DoubleValue]
           [org.apache.commons.math3.util FastMath]))

(set! *warn-on-reflection* true)
;;(set! *unchecked-math* :warn-on-boxed)

(def pc-width 160)
(def pc-height 120)
(def pixel-count (* pc-height pc-width))
(def elem-size 4)
(defn gen-data [] (float-array (take (* 4 pixel-count)  (repeatedly rand))))

(def more-data (take 100 (repeatedly gen-data)))

(def pc-data (gen-data))
(defmacro my-min [a b] `(if (< ~a ~b) ~a ~b))
(defmacro my-max [a b] `(if (< ~a ~b) ~b ~a))




(defn good-compute-bbox [^floats data]
  (let [n (alength data)]
    (loop [i (int 0)
           minx (float (aget data 0))
           maxx (float (aget data 0))
           miny (float (aget data 1))
           maxy (float (aget data 1))
           minz (float (aget data 2))
           maxz (float (aget data 2))]
      (if (< i n)
        (let [x (float (aget data (unchecked-add i 0)))
              y (float (aget data (unchecked-add i 1)))
              z (float (aget data (unchecked-add i 2)))]
          (if (and (Float/isFinite x)
                   (Float/isFinite y)
                   (Float/isFinite z))
            (recur (unchecked-add-int i (int 4))
                   (min minx x)
                   (max maxx x)
                   (min miny y)
                   (max maxy y)
                   (min minz z)
                   (max maxz z))
            (recur (unchecked-add-int i (int 4))
                   minx
                   maxx
                   miny
                   maxy
                   minz
                   maxz
                   ))
          )
        [minx maxx miny maxy minz maxz]))))


(defn primitive-compute-bbox-new [^floats data]
  (let [n (alength data)]
    (loop [i (int 0)
           minx (aget data 0)
           maxx (aget data 0)
           miny (aget data 1)
           maxy (aget data 1)
           minz (aget data 2)
           maxz (aget data 2)]
      (if (< i n)
        (let [x (aget data (unchecked-add i 0))
              y (aget data (unchecked-add i 1))
              z (aget data (unchecked-add i 2))]
          (if (and (Float/isFinite x)
                   (Float/isFinite y)
                   (Float/isFinite z))
            (recur (unchecked-add-int i 4)
                   (FastMath/min (float minx) x)
                   (FastMath/max (float maxx) x)
                   (FastMath/min (float miny) y)
                   (FastMath/max (float maxy) y)
                   (FastMath/min (float minz) z)
                   (FastMath/max (float maxz) z))
            (recur (unchecked-add-int i 4)
                   minx
                   maxx
                   miny
                   maxy
                   minz
                   maxz)))
        [minx maxx miny maxy minz maxz]))))






(defn good-pmath-compute-bbox [^floats data]
  (let [n (alength data)]
    (loop [i (int 0)
           minx (float (aget data 0))
           maxx (float (aget data 0))
           miny (float (aget data 1))
           maxy (float (aget data 1))
           minz (float (aget data 2))
           maxz (float (aget data 2))]
      (if (< i n)
        (let [x (float (aget data (unchecked-add i 0)))
              y (float (aget data (unchecked-add i 1)))
              z (float (aget data (unchecked-add i 2)))]

          (if (pmath/bool-and
               (Float/isFinite x)
               (pmath/bool-and
                (Float/isFinite y)
                (Float/isFinite z)))
            (recur (pmath/+ i 4)
                   (pmath/min (float minx) (float x))
                   (pmath/max (float maxx) (float x))
                   (pmath/min (float miny) (float y))
                   (pmath/max (float maxy) (float y))
                   (pmath/min (float minz) (float z))
                   (pmath/max (float maxz) (float z)))
            (recur (pmath/+ i (int 4))
                   minx
                   maxx
                   miny
                   maxy
                   minz
                   maxz
                   ))
          )
        [minx maxx miny maxy minz maxz]))))

(defn primitive-compute-bbox [^floats data]
  (let [n (alength data)]
    (loop [i (int 0)
           minx (float (aget data 0))
           maxx (float (aget data 0))
           miny (float (aget data 1))
           maxy (float (aget data 1))
           minz (float (aget data 2))
           maxz (float (aget data 2))]
      (if (< i n)
        (let [x (float (aget data (unchecked-add i 0)))
              y (float (aget data (unchecked-add i 1)))
              z (float (aget data (unchecked-add i 2)))]
          (if (and (Float/isFinite x)
                   (Float/isFinite y)
                   (Float/isFinite z))
            (recur (unchecked-add-int i 4)
                   (min minx x)
                   (max maxx x)
                   (min miny y)
                   (max maxy y)
                   (min minz z)
                   (max maxz z))
            (recur (unchecked-add-int i 4)
                   minx
                   maxx
                   miny
                   maxy
                   minz
                   maxz
                   ))
          )
        [minx maxx miny maxy minz maxz]))))

(defmacro my-min [a b]
  `(if (< ~a ~b) ~a ~b))

(defmacro my-max [a b]
  `(if (< ~a ~b) ~b ~a))

(defn lime-compute-bbox [^floats data]
  (let [n (alength data)]
    (loop [i (long 0)
           minx (float (aget data 0))
           maxx (float (aget data 0))
           miny (float (aget data 1))
           maxy (float (aget data 1))
           minz (float (aget data 2))
           maxz (float (aget data 2))]
      (if (< i n)
        (let [x (float (aget data (unchecked-add i 0)))
              y (float (aget data (unchecked-add i 1)))
              z (float (aget data (unchecked-add i 2)))]
          (let [[^float minx
                 ^float maxx
                 ^float miny
                 ^float maxy
                 ^float minz
                 ^float maxz]
                (if (and (Float/isFinite x)
                         (Float/isFinite y)
                         (Float/isFinite z))
                  [(min minx x)
                   (max maxx x)
                   (min miny y)
                   (max maxy y)
                   (min minz z)
                   (max maxz z)]
                  [minx
                   maxx
                   miny
                   maxy
                   minz
                   maxz])]
            (recur (unchecked-add i 4)
                   minx
                   maxx
                   miny
                   maxy
                   minz
                   maxz)))
        [minx maxx miny maxy minz maxz]))))


(defn arr-compute-bbox [^floats data]
  (let [n (alength data)
        ^floats acc (float-array [(float (aget data 0))
                                  (float (aget data 0))
                                  (float (aget data 1))
                                  (float (aget data 1))
                                  (float (aget data 2))
                                  (float (aget data 2))]
                                 )]
    (loop [i (long 0)
           ]
      (if (< i n)
        (let [x (float (aget data (unchecked-add i 0)))
              y (float (aget data (unchecked-add i 1)))
              z (float (aget data (unchecked-add i 2)))]
          (if (and (Float/isFinite x)
                   (Float/isFinite y)
                   (Float/isFinite z))
            (do
              (aset acc 0 (min (aget acc 0) x))
              (aset acc 1 (max (aget acc 1) x))
              (aset acc 2 (min (aget acc 2) y))
              (aset acc 3 (max (aget acc 3) y))
              (aset acc 4 (min (aget acc 4) z))
              (aset acc 5 (max (aget acc 5) z)))
            )
          (recur (unchecked-add i 4)))
        i))
    acc))

(defn mutable-field-compute-bbox [^floats data]
  (let [n (alength data)
        xmin (DoubleValue.)
        xmax (DoubleValue.)
        ymin (DoubleValue.)
        ymax (DoubleValue.)
        zmin (DoubleValue.)
        zmax (DoubleValue.)]
    (loop [i (long 0)
           ]
      (if (< i n)
        (let [x (float (aget data (unchecked-add i 0)))
              y (float (aget data (unchecked-add i 1)))
              z (float (aget data (unchecked-add i 2)))]
          (if (and (Float/isFinite x)
                   (Float/isFinite y)
                   (Float/isFinite z))
            (do
              (set! (.value xmin) (min (.value xmin) x))
              (set! (.value xmax) (max (.value xmax) x))
              (set! (.value ymin) (min (.value ymin) y))
              (set! (.value ymax) (max (.value ymax) y))
              (set! (.value zmin) (min (.value zmin) z))
              (set! (.value zmax) (max (.value zmax) z)))
            )
          (recur (unchecked-add i 4)))
        i))
    [(.value xmin)
     (.value xmax)
     (.value ymin)
     (.value ymax)
     (.value zmin)
     (.value zmax)
     ]))

(defmacro update-bds [[minv maxv] v]
  `(do
     (var-set ~minv (min (deref ~minv) ~v))
     (var-set ~maxv (max (deref ~maxv) ~v))))

(defn lvars-compute-bbox [^floats data]
  (let [n (alength data)]
    (with-local-vars [minx (float (aget data 0))
                      maxx (float (aget data 0))
                      miny (float (aget data 1))
                      maxy (float (aget data 1))
                      minz (float (aget data 2))
                      maxz (float (aget data 2))]
      (loop [i (long 0)]
        (if (< i n)
          (let [x (float (aget data (unchecked-add i 0)))
                y (float (aget data (unchecked-add i 1)))
                z (float (aget data (unchecked-add i 2)))]
            (if (and (Float/isFinite x)
                     (Float/isFinite y)
                     (Float/isFinite z))
              (do
                (update-bds [minx maxx] x)
                (update-bds [miny maxy] y)
                (update-bds [minz maxz] z))
              )
            (recur (unchecked-add i 4)))
          i))
      [(deref minx)
(deref maxx)
(deref miny)
(deref maxy)
(deref minz)
(deref maxz)
       ])))

(defmacro update-mut [[minv maxv] v]
  `(do
     (set! ~minv (min ~minv ~v))
     (set! ~maxv (max ~maxv ~v))))

(defn proteus-compute-bbox [^floats data]
  (let [n (alength data)]
    (let-mutable [^float minx (float (aget data 0))
                  ^float maxx (float (aget data 0))
                  ^float miny (float (aget data 1))
                  ^float maxy (float (aget data 1))
                  ^float minz (float (aget data 2))
                  ^float maxz (float (aget data 2))]
      (loop [i (long 0)]
        (if (< i n)
          (let [x (float (aget data (unchecked-add i 0)))
                y (float (aget data (unchecked-add i 1)))
                z (float (aget data (unchecked-add i 2)))]
            (if (and (Float/isFinite x)
                     (Float/isFinite y)
                     (Float/isFinite z))
              (do
                (update-mut [minx maxx] x)
                (update-mut [miny maxy] y)
                (update-mut [minz maxz] z))
              )
            (recur (unchecked-add i 4)))
          i))
      [minx
       maxx
       miny
       maxy
       minz
       maxz
       ])))

(defn java-version [x]
  (vec (BBox/computeBBox x)))

(defn benchmark-it [compute-it]
  (let [d (vec (take 100 (repeatedly gen-data)))]
    (time (doseq [x d]
            (compute-it x)))))

(defmacro benchmark-many [fns]
  `(do
     (doseq [[label#  f#] ~(mapv vector (map str fns) fns)]
       (println label#)
       (benchmark-it f#))))



(defn inspect-disassemble [fun]
  (assert (fn? fun))
  (println (nd/disassemble fun)))

(bc/defn byte-code-bbox ^"[F" [^"[F" data]
  [[:aload 1]
   [:checkcast "[F"]
   [:dup]
   [:dup]
   [:dup]
   [:arraylength]
   [:istore 2] ;; Array length
   
   [:ldc 0]
   [:faload]
   [:fstore 3] ; X
   
   [:ldc 1]
   [:faload]
   [:fstore 4] ; Y
   
   [:ldc 2]
   [:faload]
   [:fstore 5] ; Z

   [:ldc 0]
   [:istore 7] ;; Counter

   [:ldc 0]
   [:istore 8]
   
   [:mark :L/LOOP]

   ;; Check loop confidtion
   [:iload 7]
   [:iload 2]
   [:if-icmpge :L/RET]

   ;; Step the counter
   [:iload 7]
   [:ldc 4]
   [:iadd]
   [:istore 7]

   ;;;; JUNK
   [:iload 8]
   [:ldc 4]
   [:iadd]
   [:istore 8]

;;;;; JUNK
   [:goto :L/LOOP]
   [:mark :L/RET]
   
   [:aload 1]
   [:areturn]])

(defn run-benchmark []
  (benchmark-many [java-version
                   good-compute-bbox
                   primitive-compute-bbox-new
                   ]))


;; ;; What I get
;; java-version
;; "Elapsed time: 21.243212 msecs"
;; good-compute-bbox
;; "Elapsed time: 76.081984 msecs"
;; primitive-compute-bbox-new
;; "Elapsed time: 99.216807 msecs"


(comment
  (benchmark-many [;byte-code-bbox
                   java-version
                   good-compute-bbox
                   primitive-compute-bbox-new
                   ;mutable-field-compute-bbox
                   ;tuned-good-compute-bbox
                   ;good-pmath-compute-bbox
                    ;arr-compute-bbox
                   ;proteus-compute-bbox
                   ;lime-compute-bbox
                   ;lvars-compute-bbox
                   ])
  )


