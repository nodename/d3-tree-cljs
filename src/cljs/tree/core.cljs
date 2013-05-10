
(ns tree.core
      (:require [strokes :refer [d3]]))

(strokes/bootstrap)

    
(defn log [& more]
    (.log js/console (apply str more)))    
    

;; var m = [20, 120, 20, 120],
;;    w = 1280 - m[1] - m[3],
;;    h = 800 - m[0] - m[2],
;;    i = 0,
;;    root;    


(def m [20 120 20 120])
(def w (- 1280 (m 1) (m 3)))
(def h (- 800 (m 0) (m 2)))
;; not using this (def i 0)
(def root)


;; var tree = d3.layout.tree()
;;    .size([h, w]);
    
    
(def tree (-> d3 (.-layout) (.tree)
            (.size [h w])))

;; var diagonal = d3.svg.diagonal()
;;    .projection(function(d) { return [d.y, d.x]; });


(def diagonal (-> d3 (.-svg) (.diagonal)
                (.projection (fn [d] [(.-y d) (.-x d)]))))


;; var vis = d3.select("#body").append("svg:svg")
;;    .attr("width", w + m[1] + m[3])
;;    .attr("height", h + m[0] + m[2])
;;  .append("svg:g")
;;    .attr("transform", "translate(" + m[3] + "," + m[0] + ")");


(def vis (-> d3 (.select "#body") (.append "svg:svg")
           (.attr {:width (+ w (m 1) (m 3))
                   :height (+ h (m 0) (m 2))})
           
           (.append "svg:g")
           (.attr {:transform (str "translate(" (m 3) "," (m 0) ")")})))

;; // Toggle children.
;; function toggle(d) {
;;  if (d.children) {
;;    d._children = d.children;
;;    d.children = null;
;;  } else {
;;    d.children = d._children;
;;    d._children = null;
;;  }


(defn toggle [d]
  (if (.-children d)
    (do
      (set! (.-_children d) (.-children d))
      (set! (.-children d) nil))
    (do
      (set! (.-children d) (.-_children d))
      (set! (.-_children d) nil))))


;;  function toggleAll(d) {
;;    if (d.children) {
;;      d.children.forEach(toggleAll);
;;      toggle(d);
;;    }
;;  }


(defn toggle-all [d]
  (when (.-children d)
    (do
      (doseq [child (.-children d)]
        (toggle-all child))
      (toggle d))))



;; This piece replaces the global var i.
;; See its use on line 131 below.
(def next-node-id 0)
(defn get-next-node-id []
  (set! next-node-id (inc next-node-id))
  next-node-id)




;; function update(source) {

(defn update [source]
  
  
;;  var duration = d3.event && d3.event.altKey ? 5000 : 500;
  
  (let [duration (if (and (.-event d3) (.-altKey (.-event d3)))
                   5000
                   500)
        
;;  // Compute the new tree layout.
;;  var nodes = tree.nodes(root).reverse();

        nodes (.reverse (.nodes tree root))

;;  // Normalize for fixed-depth.
;;  nodes.forEach(function(d) { d.y = d.depth * 180; });

        _ (doseq [node nodes]
            (fn [d] (set! (.-y d) (* (.-depth d) 180))))

;;  // Update the nodes…
;;  var node = vis.selectAll("g.node")
;;      .data(nodes, function(d) { return d.id || (d.id = ++i); });

        node (-> vis (.selectAll "g.node")
               (.data nodes (fn [d]
                              (when (not (.-id d))
                                  (set! (.-id d) (get-next-node-id)))
                              (.-id d))))


;;  // Enter any new nodes at the parent's previous position.
;;  var nodeEnter = node.enter().append("svg:g")
;;      .attr("class", "node")
;;      .attr("transform", function(d) { return "translate(" + source.y0 + "," + source.x0 + ")"; })
;;      .on("click", function(d) { toggle(d); update(d); });

        nodeEnter (-> node (.enter) (.append "svg:g")
                    (.attr {:class "node"
                            :transform (fn [d] (str "translate(" (.-y0 source) "," (.-x0 source) ")"))})
                    (.on "click" (fn [d] (toggle d) (update d))))

;;  nodeEnter.append("svg:circle")
;;      .attr("r", 1e-6)
;;      .style("fill", function(d) { return d._children ? "lightsteelblue" : "#fff"; });

        _ (-> nodeEnter (.append "svg:circle")
            (.attr {:r 1e-6})
            (.style {:fill (fn [d] (if (.-_children d) "lightsteelblue" "#fff"))}))

;;  nodeEnter.append("svg:text")
;;      .attr("x", function(d) { return d.children || d._children ? -10 : 10; })
;;      .attr("dy", ".35em")
;;      .attr("text-anchor", function(d) { return d.children || d._children ? "end" : "start"; })
;;      .text(function(d) { return d.name; })
;;      .style("fill-opacity", 1e-6);

        _ (-> nodeEnter (.append "svg:text")
            (.attr {:x (fn [d] (if (or (.-children d) (.-_children d)) -10 10))
                    :dy ".35em"
                    :text-anchor (fn [d] (if (or (.-children d) (.-_children d)) "end" "start"))})
            (.text #(.-name %))
            (.style {:fill-opacity 1e-6}))

;;  // Transition nodes to their new position.
;;  var nodeUpdate = node.transition()
;;      .duration(duration)
;;      .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; });

        nodeUpdate (-> node (.transition)
                     (.duration duration)
                     (.attr {:transform (fn [d] (str "translate(" (.-y d) "," (.-x d) ")"))}))

;;  nodeUpdate.select("circle")
;;      .attr("r", 4.5)
;;      .style("fill", function(d) { return d._children ? "lightsteelblue" : "#fff"; });

        _ (-> nodeUpdate (.select "circle")
            (.attr {:r 4.5})
            (.style {:fill (fn [d] (if (.-_children d) "lightsteelblue" "#fff"))}))

;;  nodeUpdate.select("text")
;;      .style("fill-opacity", 1);

        _ (-> nodeUpdate (.select "text")
            (.style {:fill-opacity 1}))

;;  // Transition exiting nodes to the parent's new position.
;;  var nodeExit = node.exit().transition()
;;      .duration(duration)
;;      .attr("transform", function(d) { return "translate(" + source.y + "," + source.x + ")"; })
;;      .remove();

        nodeExit (-> node (.exit) (.transition)
                   (.duration duration)
                   (.attr {:transform (fn [d] (str "translate(" (.-y source) "," (.-x source) ")"))})
                   (.remove))

;;  nodeExit.select("circle")
;;      .attr("r", 1e-6);

        _ (-> nodeExit (.select "circle")
            (.attr {:r 1e-6}))

;;  nodeExit.select("text")
;;      .style("fill-opacity", 1e-6);

        _ (-> nodeExit (.select "text")
            (.style {:fill-opacity 1e-6}))

;;  // Update the links…
;;  var link = vis.selectAll("path.link")
;;      .data(tree.links(nodes), function(d) { return d.target.id; });

        link (-> vis (.selectAll "path.link")
               (.data (.links tree nodes) (fn [d] (.-id (.-target d)))))

;;  // Enter any new links at the parent's previous position.
;;  link.enter().insert("svg:path", "g")
;;      .attr("class", "link")
;;      .attr("d", function(d) {
;;        var o = {x: source.x0, y: source.y0};
;;        return diagonal({source: o, target: o});
;;      })
;;    .transition()
;;      .duration(duration)
;;      .attr("d", diagonal);

        _ (-> link (.enter) (.insert "svg:path" "g")
            (.attr {:class "link"
                    :d (fn [d]
                         (let [o {:x (.-x0 source) :y (.-y0 source)}]
                                 (diagonal {:source o :target o})))})
            (.transition)
            (.duration duration)
            (.attr {:d diagonal}))

;;  // Transition links to their new position.
;;  link.transition()
;;      .duration(duration)
;;      .attr("d", diagonal);

        _ (-> link (.transition)
            (.duration duration)
            (.attr {:d diagonal}))

;;  // Transition exiting nodes to the parent's new position.
;;  link.exit().transition()
;;      .duration(duration)
;;      .attr("d", function(d) {
;;        var o = {x: source.x, y: source.y};
;;        return diagonal({source: o, target: o});
;;      })
;;      .remove();

        _ (-> link (.exit) (.transition)
            (.duration duration)
             (.attr {:d (fn [d]
                          (let [o {:x (.-x source) :y (.-y source)}]
                                  (diagonal {:source o :target o})))})
             (.remove))

;;  // Stash the old positions for transition.
;;  nodes.forEach(function(d) {
;;    d.x0 = d.x;
;;    d.y0 = d.y;
;;  });

        _ (doseq [node nodes]
            (set! (.-x0 node) (.-x node))
            (set! (.-y0 node) (.-y node)))]
;;}

    ))


;; d3.json("flare.json", function(json) {
;;  root = json;
;;  root.x0 = h / 2;
;;  root.y0 = 0;

;;  // Initialize the display to show a few nodes.
;;  root.children.forEach(toggleAll);
;;  toggle(root.children[1]);
;;  toggle(root.children[1].children[2]);
;;  toggle(root.children[9]);
;;  toggle(root.children[9].children[0]);

;;  update(root);
;; });


(.json d3 "flare.json" (fn [json]
                         (set! root json)
                         (set! (.-x0 root) (/ h 2))
                         (set! (.-y0 root) 0)
                         
                         (doseq [child (.-children root)]
                           (toggle-all child))
                         (let [child (aget (.-children root) 1)]
                           (toggle child)
                           (toggle (aget (.-children child) 2)))
                         (let [child (aget (.-children root) 9)]
                           (toggle child)
                           (toggle (aget (.-children child) 0)))
                         
                         (update root)))