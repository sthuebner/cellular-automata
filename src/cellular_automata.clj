;;;; based on Lau Jensen's blog posts
;;;; - http://www.bestinclass.dk/index.clj/2009/10/brians-functional-brain.html
;;;; - http://www.bestinclass.dk/index.clj/2009/10/brians-transient-brain.html
;;;;
;;;; and JĀNIS DŽERIŅŠ' post http://t-b-o-g.blogspot.com/2009/10/brians-brain-on-clojure.html
;;;; (and its follow ups)
;;;;
;;;; some more tweaks by me

(ns cellular-automata
  (:import (javax.swing JFrame JPanel)
	   (java.awt Color Graphics2D)
	   (java.awt.image BufferedImage)))

(defstruct board :width :height :cells)

(defn board-size [board]
  (* (:width board) (:height board)))

(defn coords [board]
  (for [y (range (:height board)) x (range (:width board))] [x y]))

(defn cell-state [board idx]
  ((:cells board) idx))

(defn make-random-board [w h]
  (let [board-size (* w h)]
    (struct board w h
	    (vec
	     (take board-size
		   (repeatedly
		    #(if (< 50 (rand-int 100)) :on :off)))))))

(defn make-board [w h]
  (let [board-size (* w h)]
    (struct board w h
	    (-> (vec (repeat board-size :off))
		(assoc (dec (quot w 2)) :on)
		(assoc (quot w 2) :on)
		(assoc 0 :on)
		(assoc (- w 1) :on)))))

(def neighbors-idx
     (memoize
      (fn [w h idx]
	(let [left? (= 0 (rem idx w))
	      right? (= (dec w) (rem idx w))
	      top? (= 0 (quot idx w))
	      bottom? (= (dec h) (quot idx w))]
	  [(+ idx -1 (if left? w 0))
	   (+ idx 1 (if right? (- w) 0))
	   (+ idx -1 (if left? 0 (- w)) (if top? (* w h) 0))
	   (+ idx (- w) (if top? (* w h) 0))
	   (+ idx 1 (- w) (if right? (- w) 0) (if top? (* w h) 0))
	   (+ idx -1 w (if left? w 0) (if bottom? (- (* w h)) 0))
	   (+ idx w (if bottom? (- (* w h)) 0))
	   (+ idx 1 (if right? 0 w) (if bottom? (- (* w h)) 0))]))))

(defn neighbors [{:keys [cells width height]} idx]
  (map #(nth cells %)
       (neighbors-idx width height idx)))

(defn on-neighbors [board idx]
  (count (filter #(= :on %) (neighbors board idx))))

(defn brian-rules [board idx]
  (let [self (cell-state board idx)]
    (cond
      (= :on    self)                    :dying
      (= :dying self)                    :off
      (= 2 (on-neighbors board idx))     :on
      :else                              :off)))

(defn conway-rules [board idx]
  (let [self (cell-state board idx)
	neighbors (on-neighbors board idx)]
    (cond
     (and (= :off self) (= 3 neighbors)) :on
     (= :off self) :off
     (> 2 neighbors) :off
     (< 3 neighbors) :off
     :else :on)))

(defn step [stage rules]
  (struct board (:width stage) (:height stage)
	  (->> (range (board-size stage))
	       (map #(rules stage %))
	       vec)))


(defonce keep-going (atom false))

(defn activity-loop [#^JPanel surface stage rules]
  (while @keep-going
    (swap! stage step rules)
    (.repaint surface)))

(defn render-cell [#^Graphics2D g [x-scale y-scale] state [x y]]
  (let [x  (inc (* x x-scale))
        y  (inc (* y y-scale))]
    (doto g
      (.setColor (if (= state :dying) Color/GRAY Color/WHITE))
      (.fillRect x y (dec x-scale) (dec y-scale)))))

(defn render [#^Graphics2D g img #^Graphics2D bg [width height] dim-scale stage]
  (.setColor bg Color/BLACK)
  (.fillRect bg 0 0 width height)
  (dorun (map (fn [cell-state coords]
		(when (not= :off cell-state)
		  (render-cell bg dim-scale cell-state coords)))
	      (:cells stage)
	      (coords stage)))
  (.drawImage g img 0 0 nil))

(defn start-world
  ([]
     (start-world brian-rules))
  ([rules]
     (start-world rules (make-random-board 120 120)))
  ([rules board]
     (let [stage (atom board)
	   dim-screen [640 640]
	   dim-scale [(/ (dim-screen 0) (:width @stage))
		      (/ (dim-screen 1) (:height @stage))]
	   frame (JFrame.)
	   img   (BufferedImage. (dim-screen 0) (dim-screen 1) (BufferedImage/TYPE_INT_ARGB))
	   bg    (.getGraphics img)
	   panel (doto (proxy [JPanel] [] (paint [g] (render g img bg dim-screen dim-scale @stage))))]
       (doto frame (.add panel) .pack (.setSize (dim-screen 0) (dim-screen 1)) .show
	     (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
       (swap! keep-going (fn [kg] true))
       (future
	(activity-loop panel stage rules)
	(.dispose frame)))))

(defn stop-world []
  (swap! keep-going (fn [kg] false)))
