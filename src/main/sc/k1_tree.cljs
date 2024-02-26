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

    (nil? tree) {:key k :value v :below 0}    ;; new node

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

(defn- nearest
  "Find the nearest node to `k`."
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
    (f tree nil)))

(defn nearest-key
  "Find the key in the tree which is nearest to `k`."
  [tree k]
  (:key (nearest tree k)))

(defn nearest-value
  "Find the value in the tree which is nearest to `k`."
  [tree k]
  (:value (nearest tree k)))

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

(def pos-inf js/Number.POSITIVE_INFINITY)
(def neg-inf js/Number.NEGATIVE_INFINITY)

(defn keys-in-range-coalescing
  "Find the keys in the range [`from`, `to`], coalescing keys which are
  closer than `size` into an interval.

  Returns a list of keys/intervals."
  [tree from to size]
  (letfn [(f [node from to l r]
            (let [mid (:key node)]
              (cond
                (< (- r l) size) [[:interval [l r]]]

                :else
                (-> (if (includes? from to mid) [[:key (:key node)]] [])
                    (into (when (and (:left node) (< from mid))
                            (f (:left node) from to l mid)))
                    (into (when (and (:right node) (> to mid))
                            (f (:right node) from to mid r)))))))]
    (f tree from to neg-inf pos-inf)))

(defn n-keys
  "Find the number of keys in the tree."
  [tree]
  (+ 1
     (when (:left tree)
       (n-keys (:left tree)))
     (when (:right tree)
       (n-keys (:right tree)))))
