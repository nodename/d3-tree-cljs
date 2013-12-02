(ns tree.core
      (:require [clojure.zip :as zip]
                [strokes :refer [d3]]
                ))

(defn log [& more]
  (.log js/console (apply str more)))

(defn prettify [obj]
  (js/JSON.stringify obj nil 2))

(defn jsonlog [obj]
  (log (prettify obj)))

(strokes/bootstrap)
    
(def tree-canvas-width 1280)
(def tree-canvas-height 800)
(def margin {:top 20 :left 120 :bottom 20 :right 120})
(def tree-width (- tree-canvas-width (margin :left) (margin :right)))
(def tree-height (- tree-canvas-height (margin :top) (margin :bottom)))


;; allowed values for orientation: vertical or horizontal
(def vertical {:x-prop "x" :y-prop "y" :x0-prop "x0" :y0-prop "y0" :dx-prop "dx"})
(def horizontal {:x-prop "y" :y-prop "x" :x0-prop "y0" :y0-prop "x0" :dx-prop "dy"})

(def orientation vertical)


;; allowed values for draw-link: line-segment or bezier
(def line (-> d3 (.-svg) (.line) (.x (fn [d] (aget d (orientation :x-prop)))) (.y (fn [d] (aget d (orientation :y-prop))))))
(def line-segment (fn [spec]
            (let [source (aget spec "source")
                  target (aget spec "target")
                  data [source target]]
              (line data))))
(def bezier (-> d3 (.-svg) (.diagonal)
                (.projection (fn [d] [(aget d (orientation :x-prop)) (aget d (orientation :y-prop))]))))

(def draw-link line-segment)


(def tree-canvas (-> d3 (.select "#body") (.append "svg:svg")
           (.attr "width" tree-canvas-width)
           (.attr "height" tree-canvas-height)
           (.append "svg:g")
           (.attr "transform" (str "translate(" (margin :left) "," (margin :top) ")"))))

(def control-canvas (-> d3 (.select "#body") (.append "svg:svg")
           (.attr "width" (+ tree-canvas-width 500))
           (.attr "height" tree-canvas-height)
           (.append "svg:g")
           (.attr "transform" (str "translate(" tree-canvas-width ",0)"))))

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


(defn json-zip
  [root]
  (let [branch? (constantly true)
        children (fn [obj] (seq (aget obj "children")))
        make-node (fn [node children]
                    (aset node "children" (and children (apply vector children)))
                    node)]
    (zip/zipper branch?
                children
                make-node
                root)))

;; redraw refactoring as in http://weblog.bocoup.com/reusability-with-d3

;; source-node is the data node on which the event was triggered
(defn redraw-nodes [node-selection source-node duration click-handler]
  (let [fill-color #(if (aget % "hidden-children") "lightsteelblue" "#fff")
        
        entering (-> node-selection (.enter) (.append "svg:g"))
        
        ;; Enter any new nodes at source-node's previous position.
        enter (fn [node]
                     (-> node
                       (.attr "class" "node")
                       (.attr "transform" (fn [d] (str "translate(" (aget source-node (orientation :x0-prop)) "," (aget source-node (orientation :y0-prop)) ")")))
                       (.on "click" click-handler))
                     (let [circle (-> node (.append "svg:circle"))]
                       (-> circle
                         (.attr "r" 1e-6)
                         (.style "fill" fill-color)))
                     (let [text (-> node (.append "svg:text"))]
                       (-> text
                         (.attr (orientation :y-prop) (fn [d] (if (or (aget d "children") (aget d "hidden-children")) -10 10)))
                         (.attr (orientation :dx-prop) ".35em")
                         (.attr "text-anchor" #(if (or (aget % "children") (aget % "hidden-children")) "end" "start"))
                         (.text #(aget % "name"))
                         (.style "fill-opacity" 1e-6))))
        
        updating-transition (-> node-selection (.transition))
        
        ;; Transition nodes to their new position.
        transition-update (fn [transition]
                                 (-> transition
                                   (.duration duration)
                                   (.attr "transform" (fn [d] (str "translate(" (aget d (orientation :x-prop)) "," (aget d (orientation :y-prop)) ")"))))
                                 (let [circle (-> transition (.select "circle"))]
                                   (-> circle
                                     (.attr "r" 4.5)
                                     (.style "fill" fill-color)))
                                 (let [text (-> transition (.select "text"))]
                                   (-> text
                                     (.style "fill-opacity" 1))))
                                 
        
        exiting-transition (-> node-selection (.exit) (.transition)) ;; externed exit!
        
        ;; Transition exiting nodes to source-node's new position.
        transition-exit (fn [transition]
                               (-> transition 
                                 (.duration duration)
                                 (.attr "transform" (fn [d] (str "translate(" (aget source-node (orientation :x-prop)) "," (aget source-node (orientation :y-prop)) ")")))
                                 (.remove))
                               (let [circle (-> transition (.select "circle"))]
                                 (-> circle
                                   (.attr "r" 1e-6)))
                               (let [text (-> transition (.select "text"))]
                                 (-> text
                                   (.style "fill-opacity 1e-6"))))]
    
    (-> entering enter)
    
    (-> updating-transition transition-update)
    
    (-> exiting-transition transition-exit)))


(defn redraw-links [link-selection source-node duration]
  (let [entering (-> link-selection (.enter) (.insert "svg:path" "g"))
        
        ;; Enter any new links at source-node's previous position.
        enter (fn [link]
                     (-> link
                       (.attr "class" "link")
                       (.attr "d" #(let [o (js-obj "x" (aget source-node "x0") "y" (aget source-node "y0"))]
                                     (draw-link (js-obj "source" o "target" o))))))
        
        entering-transition (-> entering (.transition))
        
        updating-transition (-> link-selection (.transition))
        
        ;; Transition links to their new position (for entering-transition and updating-transition)
        transition-update (fn [transition]
                                 (-> transition
                                   (.duration duration)
                                   (.attr "d" draw-link)))
        
        exiting-transition (-> link-selection (.exit) (.transition))
        
        ;; Transition exiting links to source-node's new position.
        transition-exit (fn [transition]
                               (-> transition
                                 (.duration duration)
                                 (.attr "d" #(let [o (js-obj "x" (aget source-node "x") "y" (aget source-node "y"))]
                                               (draw-link (js-obj "source" o "target" o))))
                                 (.remove)))]
    
    (-> entering enter)
    
    (-> entering-transition transition-update)
    
    (-> updating-transition transition-update)
    
    (-> exiting-transition transition-exit)))
    

        
(defn redraw [source-node zipper tree-layout]
  
  (let [duration (if (and (aget d3 "event") (aget (aget d3 "event") "altKey"))
                   5000
                   500)
        
        click-handler (fn [d] (toggle d) (redraw d zipper tree-layout))
        
        root (zip/root zipper)
        
        ;; Compute the new tree layout.
        nodes (.reverse ((aget tree-layout "nodes") root))
        
        node-selection (-> tree-canvas (.selectAll "g.node")
                         (.data nodes (fn [d]
                                        (when (not (aget d "id"))
                                          (aset d "id" (get-next-node-id)))
                                        (aget d "id"))))
        
        link-selection (-> tree-canvas (.selectAll "path.link")
                         (.data (.links tree-layout nodes) #(aget (aget % "target") "id")))]
        
    ;; Normalize for fixed-depth.
    (doseq [node nodes]
      (aset node (orientation :y-prop) (* (aget node "depth") 180)))
    
    (redraw-nodes node-selection source-node duration click-handler)
    (redraw-links link-selection source-node duration)
    
    ;; Stash the old positions for transition.
    (doseq [node nodes]
      (aset node "x0" (aget node "x"))
      (aset node "y0" (aget node "y")))))



(defn draw-control [command y]
  (let [node (-> control-canvas (.append "svg:g")
               (.attr "class" "node")
               (.attr "transform" #(str "translate(0," y ")"))
               (.attr "name" (name command))
            ;   (.on "click" control-click-handler)
               )
        ]
    
    (-> node (.append "svg:circle")
      (.attr "r" 4.5))
    
    (-> node (.append "svg:text")
      (.attr "y" 10)
      (.attr "dx" ".35em")
      (.attr "text-anchor" "start")
      (.text (name command))
      (.style "fill-opacity" 1))))


(defn draw-controls []
  (doseq [[command y] (map list [:left :right :leftmost :rightmost :down :up :root] [0 20 40 60 80 100 120])]
    (draw-control command y)))



(defn draw [data]
  (draw-controls)
  (let [root data
        _ (aset root "x0" (/ tree-width 2))
        _ (aset root "y0" 0)
        
        zipper (-> data json-zip)
        
        tree-layout (-> d3 (.-layout) (.tree)
               (.size (if (== orientation vertical) [tree-width tree-height] [tree-height tree-width])))]
    
    ; Initialize the display to show a few nodes.
    (doseq [child (aget root "children")]
      (toggle-all child))
    (let [child (aget (aget root "children") 1)]
      (toggle child)
      (toggle (aget (aget child "children") 2)))
    (let [child (aget (aget root "children") 9)]
      (toggle child)
      (toggle (aget (aget child "children") 0)))
    
    (redraw root zipper tree-layout)))

(.json d3 "flare.json" draw)
