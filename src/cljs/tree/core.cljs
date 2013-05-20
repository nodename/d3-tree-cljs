(ns tree.core
      (:require [clojure.zip :as zip]
                [strokes :refer [d3]]))

(defn log [& more]
    (.log js/console (apply str more)))    
    

(strokes/bootstrap)
    
(def svg-width 1280)
(def svg-height 800)
(def margin {:top 20 :left 120 :bottom 20 :right 120})
(def tree-width (- svg-width (margin :left) (margin :right)))
(def tree-height (- svg-height (margin :top) (margin :bottom)))



(def vertical {:x-prop "x" :y-prop "y" :x0-prop "x0" :y0-prop "y0" :dx-prop "dx"})
(def horizontal {:x-prop "y" :y-prop "x" :x0-prop "y0" :y0-prop "x0" :dx-prop "dy"})

(def orientation vertical)



(def line (-> d3 (.-svg) (.line) (.x (fn [d] (aget d (orientation :x-prop)))) (.y (fn [d] (aget d (orientation :y-prop))))))
(def line-segment (fn [spec]
            (let [source (aget spec "source")
                  target (aget spec "target")
                  data [source target]]
              (line data))))
(def bezier (-> d3 (.-svg) (.diagonal)
                (.projection (fn [d] [(aget d (orientation :x-prop)) (aget d (orientation :y-prop))]))))

(def draw-link line-segment)


(def svg (-> d3 (.select "#body") (.append "svg:svg")
           (.attr "width" svg-width)
           (.attr "height" svg-height)
           (.append "svg:g")
           (.attr "transform" (str "translate(" (margin :left) "," (margin :top) ")"))))


(defn toggle [d]
  (if (aget d "children")
    (do
      (aset d "hidden-children" (aget d "children"))
      (aset d "children" nil))
    (do
      (aset d "children" (aget d "hidden-children"))
      (aset d "hidden-children" nil))))


(defn toggle-all [d]
  (when (aget d "children")
      (doseq [child (aget d "children")]
        (toggle-all child))
      (toggle d)))


(def next-node-id 0)
(defn get-next-node-id []
  (set! next-node-id (inc next-node-id))
  next-node-id)


(defn update [source root tree]
  
  (let [duration (if (and (aget d3 "event") (aget (aget d3 "event") "altKey"))
                   5000
                   500)
        
        click-handler (fn [d] (toggle d) (update d root tree))
        
;;  // Compute the new tree layout.
        nodes (.reverse ((aget tree "nodes") root))

;;  // Update the nodes…
        node (-> svg (.selectAll "g.node")
               (.data nodes (fn [d]
                              (when (not (aget d "id"))
                                  (aset d "id" (get-next-node-id)))
                              (aget d "id"))))

;;  // Enter any new nodes at the parent's previous position.
        nodeEnter (-> node (.enter) (.append "svg:g")
                    (.attr "class" "node")
                    (.attr "transform" #(str "translate(" (aget source (orientation :x0-prop)) "," (aget source (orientation :y0-prop)) ")"))
                    (.on "click" click-handler))

;;  // Transition nodes to their new position.
        nodeUpdate (-> node (.transition)
                     (.duration duration)
                     (.attr "transform" #(str "translate(" (aget % (orientation :x-prop)) "," (aget % (orientation :y-prop)) ")")))

;;  // Transition exiting nodes to the parent's new position.
        nodeExit (-> node (.exit) (.transition) ;; externed exit!
                   (.duration duration)
                   (.attr "transform" #(str "translate(" (aget source (orientation :x-prop)) "," (aget source (orientation :y-prop)) ")"))
                   (.remove))

;;  // Update the links…
        link (-> svg (.selectAll "path.link")
               (.data (.links tree nodes) #(aget (aget % "target") "id")))]

;;  // Normalize for fixed-depth.
        (doseq [node nodes]
          #(aset % (orientation :y-prop) (* (aget % "depth") 10)))


        (-> nodeEnter (.append "svg:circle")
          (.attr "r" 1e-6)
          (.style "fill" #(if (aget % "hidden-children") "lightsteelblue" "#fff")))

        (-> nodeEnter (.append "svg:text")
          (.attr (orientation :y-prop) #(if (or (aget % "children") (aget % "hidden-children")) -10 10))
          (.attr (orientation :dx-prop) ".35em")
          (.attr "text-anchor" #(if (or (aget % "children") (aget % "hidden-children")) "end" "start"))
          (.text #(aget % "name"))
          (.style "fill-opacity" 1e-6))

        (-> nodeUpdate (.select "circle")
          (.attr "r" 4.5)
          (.style "fill" #(if (aget % "hidden-children") "lightsteelblue" "#fff")))

        (-> nodeUpdate (.select "text")
          (.style "fill-opacity" 1))

        (-> nodeExit (.select "circle")
          (.attr "r" 1e-6))

        (-> nodeExit (.select "text")
          (.style "fill-opacity 1e-6"))

;;  // Enter any new links at the parent's previous position.
        (-> link (.enter) (.insert "svg:path" "g")
          (.attr "class" "link")
          (.attr "d" #(let [o (js-obj "x" (aget source "x0") "y" (aget source "y0"))]
                        (draw-link (js-obj "source" o "target" o))))
          (.transition)
          (.duration duration)
          (.attr "d" draw-link))

;;  // Transition links to their new position.
        (-> link (.transition)
          (.duration duration)
          (.attr "d" draw-link))

;;  // Transition exiting nodes to the parent's new position.
        (-> link (.exit) (.transition)
          (.duration duration)
          (.attr "d" #(let [o (js-obj "x" (aget source "x") "y" (aget source "y"))]
                        (draw-link (js-obj "source" o "target" o))))
          (.remove))

;;  // Stash the old positions for transition.
        (doseq [node nodes]
          (aset node "x0" (aget node "x"))
          (aset node "y0" (aget node "y")))))


(defn draw [json]
  (let [root json
        tree (-> d3 (.-layout) (.tree)
               (.size (if (== orientation vertical) [tree-width tree-height] [tree-height tree-width])))]
    (aset root "x0" (/ tree-width 2))
    (aset root "y0" 0)
                           
;;  Initialize the display to show a few nodes.
    (doseq [child (aget root "children")]
      (toggle-all child))
    (let [child (aget (aget root "children") 1)]
      (toggle child)
      (toggle (aget (aget child "children") 2)))
    (let [child (aget (aget root "children") 9)]
      (toggle child)
      (toggle (aget (aget child "children") 0)))
    
    (update root root tree)))

(.json d3 "flare.json" draw)