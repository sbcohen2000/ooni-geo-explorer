(ns sc.canvas)

(defmacro with-offset
  [ofs ctx & body]
  (let [ctx-nm (gensym 'ctx)
        [x y] ofs]
    `(let [~ctx-nm ~ctx]
       (do
         (.save ~ctx-nm)
         (.translate ~ctx-nm ~x ~y)
         ~@body
         (.restore ~ctx-nm)))))
