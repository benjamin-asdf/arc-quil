(ns
    arcvis.arcvis
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.data.json :as json]
   [ftlm.vehicles.art.lib :refer [*dt*] :as lib]
   [quil.core :as q]))

(def max-grid-size 30)

(defn setup []
  (q/frame-rate 20))

(defn
  read-data
  [f]
  (json/read-str
   (slurp f)
   :key-fn
   keyword))

(defonce state (atom nil))

(defn rand-train-dat []
  (rand-nth
   (into
    []
    (filter
     (comp
      #(str/ends-with? % "json")
      str)
     (file-seq
      (io/file
       "/home/benj/repos/ARC-AGI/data/training/"))))))

;; (def colors
;;   [[256 0 0]
;;    [0 256 0]
;;    [0 0 256]])

(def color
  (memoize
   (fn [i] (repeatedly 3 #(rand-int 256)))))

(defn arc-grid
  [{:keys [data width height pos title]}]
  (q/with-translation
      pos
      (q/text-size 25)
      (q/with-fill 0 (q/text title 20 20))
      (q/with-translation
          [0 30]
          (doall
           (for [[row-idx row] (map-indexed vector data)]
             (doall
              (for [[col-idx item] (map-indexed vector row)]
                (q/with-translation
                    [(* col-idx (/ width (count row)))
                     (* row-idx (/ height (count data)))]
                    (q/with-fill
                        (color item)
                        (q/rect 0
                                0
                                (/ width (count row))
                                (/ width (count row))))))))))))



(defn
  every-n-seconds
  [n op]
  (let [s (atom {:last n})]
    (fn
      []
      (swap! s - *dt*)
      (when (<= (:last @s) 0) (op)))))

(defn
  n-seconds-cooldown
  [n op]
  (let [next-allowed-in (atom n)]
    (fn
      []
      (println *dt*
               (swap! next-allowed-in - *dt*))
      (when
          (<= @next-allowed-in 0)
          (reset! next-allowed-in n)
          (op)))))

(defn
  select-next!
  []
  (swap!
   state
   assoc
   :data
   (read-data (rand-train-dat))))


(def
  user-select-next
  (n-seconds-cooldown 1 select-next!))

(defn
  draw
  []
  (let [current-tick (q/millis)
        dt (/
            (-
             current-tick
             (:last-tick @state 0))
            1000.0)
        _ (swap! state assoc :last-tick current-tick)

        ]
    (binding
        [*dt* dt]
        (q/stroke 0)
        (q/background 200)
        (q/stroke-weight 0.5)
        (q/fill 256 0 0)
        (when
            (q/key-pressed?)
            (case
                (q/key-code)
                76
                (user-select-next)
                nil))
        (when-let
            [data
             (first (:train (:data @state)))]
            (let [width 300
                  height 300]
              (q/with-translation
                  [10 10]
                  (arc-grid
                   {:data (:input data)
                    :pos [0 0]
                    :width width
                    :title "input"
                    :height height})
                  (arc-grid
                   {:data (:output data)
                    :pos [0 (+ (* height 1) 50)]
                    :width width
                    :title "output"
                    :height height})))))))

(q/sketch
 :settings #(q/smooth 2)
 :setup #'setup
 :draw #'draw
 ;; :size [600 600]
 :size [800 800])

(comment
  {:test
   [{:input
     [[7 0 7] [7 0 7] [7 7 0]]
     :output [[7 0 7 0 0 0 7 0 7]
              [7 0 7 0 0 0 7 0 7]
              [7 7 0 0 0 0 7 7 0]
              [7 0 7 0 0 0 7 0 7]
              [7 0 7 0 0 0 7 0 7]
              [7 7 0 0 0 0 7 7 0]
              [7 0 7 7 0 7 0 0 0]
              [7 0 7 7 0 7 0 0 0]
              [7 7 0 7 7 0 0 0 0]]}]
   :train
   [{:input [[0 7 7] [7 7 7] [0 7 7]]
     :output [[0 0 0 0 7 7 0 7 7]
              [0 0 0 7 7 7 7 7 7]
              [0 0 0 0 7 7 0 7 7]
              [0 7 7 0 7 7 0 7 7]
              [7 7 7 7 7 7 7 7 7]
              [0 7 7 0 7 7 0 7 7]
              [0 0 0 0 7 7 0 7 7]
              [0 0 0 7 7 7 7 7 7]
              [0 0 0 0 7 7 0 7 7]]}
    {:input [[4 0 4] [0 0 0] [0 4 0]]
     :output [[4 0 4 0 0 0 4 0 4]
              [0 0 0 0 0 0 0 0 0]
              [0 4 0 0 0 0 0 4 0]
              [0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0]
              [0 0 0 4 0 4 0 0 0]
              [0 0 0 0 0 0 0 0 0]
              [0 0 0 0 4 0 0 0 0]]}
    {:input [[0 0 0] [0 0 2] [2 0 2]]
     :output [[0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 2]
              [0 0 0 0 0 0 2 0 2]
              [0 0 0 0 0 0 0 0 0]
              [0 0 2 0 0 0 0 0 2]
              [2 0 2 0 0 0 2 0 2]]}
    {:input [[6 6 0] [6 0 0] [0 6 6]]
     :output [[6 6 0 6 6 0 0 0 0]
              [6 0 0 6 0 0 0 0 0]
              [0 6 6 0 6 6 0 0 0]
              [6 6 0 0 0 0 0 0 0]
              [6 0 0 0 0 0 0 0 0]
              [0 6 6 0 0 0 0 0 0]
              [0 0 0 6 6 0 6 6 0]
              [0 0 0 6 0 0 6 0 0]
              [0 0 0 0 6 6 0 6 6]]}
    {:input [[2 2 2] [0 0 0] [0 2 2]]
     :output [[2 2 2 2 2 2 2 2 2]
              [0 0 0 0 0 0 0 0 0]
              [0 2 2 0 2 2 0 2 2]
              [0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0]
              [0 0 0 2 2 2 2 2 2]
              [0 0 0 0 0 0 0 0 0]
              [0 0 0 0 2 2 0 2 2]]}]})






;; effectors ideas:
;; -------------------
;;
;;
;; 1. scale
;;  - more grid elements
;; 2. eye move
;; 3. gravity
;; 4. fill
;; 5. flip colors
;; 6. rotate
;; 7.


;;
;;


(read-data
 (first (into
         []
         (filter
          (comp
           #(str/ends-with? % "json")
           str)
          (file-seq
           (io/file
            "/home/benj/repos/ARC-AGI/data/training/"))))))


(->>
 (into []
       (filter (comp #(str/ends-with? % "json") str)
               (file-seq
                (io/file
                 "/home/benj/repos/ARC-AGI/data/training/"))))
 (map read-data)
 (mapcat (juxt :train :test))
 (mapcat identity)
 ;; (mapcat
 ;;  (juxt :input :output))
 ;; (map (juxt count (comp count first)))
 first)




(map #(apply max %)
     (apply map vector
            (->>
             (into []
                   (filter (comp #(str/ends-with? % "json") str)
                           (file-seq
                            (io/file
                             "/home/benj/repos/ARC-AGI/data/training/"))))
             (map read-data)
             (map :train)
             (mapcat identity)
             ;; (map :input)
             (mapcat (juxt :input :output))
             (map (juxt count (comp count first))))))


(map #(apply max %)
     (apply map vector
            (->>
             (into []
                   (filter (comp #(str/ends-with? % "json") str)
                           (file-seq
                            (io/file
                             "/home/benj/repos/ARC-AGI/data/training/"))))
             (map read-data)
             (map :test)
             (mapcat identity)
             ;; (map :input)
             (mapcat (juxt :input :output))
             (map (juxt count (comp count first))))))
