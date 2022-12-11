(ns advent-of-clerk.utils)

(def grid-viewer
  {:pred string?
   :render-fn '(fn [s]
                 (into [:div.flex.flex-col]
                       (map (fn [l]
                              (into [:div.flex.inline-flex]
                                    (map (fn [c]
                                           [:div.inline-block {:style {:width 16 :height 16 :border "1px solid grey"}
                                                               :class (if (= c \#) "bg-black" "bg-white")}])
                                         l)))
                            (str/split-lines s))))})
