(ns sc.k1-tree)

(defn k1-tree
  "Make a new empty k1-tree."
  []
  nil)

(defn- add-point
  "Add a point at `k` into the tree with value `v`."
  [tree k v]
  (cond
    (= k (:key tree)) (assoc tree :value v)   ;; replace value

    (nil? tree) {:key k :value v}             ;; new node

    (< k (:key tree))                         ;; traverse left
    (assoc tree :left (add-point (:left tree) k v))

    :else                                     ;; traverse right
    (assoc tree :right (add-point (:right tree) k v))))

(defn add-points
  [tree kvs]
  ;; We shuffle the key-value pairs here to try and get a uniform
  ;; distribution of points, thereby improving our query runtime.
  (let [shuffled (shuffle kvs)]
    (reduce (fn [t [k v]] (add-point t k v)) tree shuffled)))

(defn- leaf?
  "Is the current node a leaf?"
  [node]
  (and (nil? (:left node))
       (nil? (:right node))))

(defn- best-of
  "Return the node (`a` or `b`) whose key is closer to `k`."
  [a b k]
  (if (or (nil? a) (nil? b))
    (or a b)
    (let [d-a (abs (- (:key a) k))
          d-b (abs (- (:key b) k))]
      (if (< d-a d-b) a b))))

(defn nearest-point
  "Find the point in the tree which is nearest to `k`."
  [tree k]
  (letfn [(f [node best]
            (if (leaf? node)
              (best-of node best k)
              (let [mid           (:key node)
                    side          (if (< k mid) :left :right)
                    opposite-side (if (= :left side) :right :left)
                    ;; best of subtrees
                    best' (f (side node) best)
                    ;; best of subtrees and this node
                    best'' (best-of node best' k)
                    nearest-distance (abs (- (:key best'') k))
                    distance-to-mid  (abs (- k mid))]
                (if (> nearest-distance distance-to-mid)
                  ;; the nearest distance crosses the separating
                  ;; plane, so we have to check the other branch too.
                  (f (opposite-side node) best'')
                  best''))))]
    (:value (f tree nil))))

(defn- includes?
  "Check if the input point `x` is included in the interval given
  by [`from`, `to`]."
  [from to x]
  (and (>= x from) (<= x to)))

(defn points-in-range
  "Find the points in the range [`from`, `to`]."
  [tree from to]
  (let [mid (:key tree)]
    (-> (if (includes? from to mid) [(:value tree)] [])
        (into (when (and (:left tree) (< from mid))
                (points-in-range (:left tree) from to)))
        (into (when (and (:right tree) (> to mid))
                (points-in-range (:right tree) from to))))))
