(ns tictac.core
  (:require
   [reagent.core :as r])
  (:require-macros
   [devcards.core :refer [defcard-rg defcard-doc deftest]]
   [cljs.test :refer [is]]))

(enable-console-print!)

(def empty-board
  {:player :x})

(def other-player
  {:x :o
   :o :x})

(def color-string
  {:x "X"
   :o "O"})

(defn color [board point]
  (get-in board [:board point]))

(defn move-at [board point]
  (-> board
      (assoc-in [:board point] (:player board))
      (update :player other-player)))

(defn rows []
  [[[0 0] [0 1] [0 2]]
   [[1 0] [1 1] [1 2]]
   [[2 0] [2 1] [2 2]]
   [[0 0] [1 0] [2 0]]
   [[0 1] [1 1] [2 1]]
   [[0 2] [1 2] [2 2]]
   [[0 0] [1 1] [2 2]]
   [[0 2] [1 1] [2 0]]])

(deftest test-rows
  (is (= (count (rows))
         8)))

(defn won? [board row player]
  (every? (fn [pt]
            (= (color board pt)
               player))
          row))

(deftest test-won?
  (is (won? {:board {[0 0] :x
                     [1 1] :x
                     [2 2] :x}}
            [[0 0] [1 1] [2 2]]
            :x))
  (is (not (won? {:board {[0 0] :x
                          [1 1] :x
                          [2 2] :x}}
                 [[0 0] [0 1] [0 2]]
                 :x))))

(defn winner [board]
  (some (fn [row]
          (cond (won? board row :x)
                :x
                (won? board row :o)
                :o))
        (rows)))

(deftest test-winner
  (is (nil? (winner empty-board)))
  (is (= :x (winner {:board {[0 0] :x
                             [1 1] :x
                             [2 2] :x}})))
  (is (= :o (winner {:board {[1 0] :o
                             [1 1] :o
                             [1 2] :o}}))))

(defonce game-state (r/atom empty-board))

(defn new-game! []
  (reset! game-state empty-board))

(defn move-at! [point]
  (swap! game-state move-at point))

(defn board [state]
  (let [winner (winner state)]
    [:div
     [:div.board
      (for [row (range 3)]
        ^{:key row}
        [:div.row
         (for [col (range 3)]
           (let [point [row col]
                 color (get-in state [:board point])]
             ^{:key [row col]}
             [:div.cell (when-not (or color winner)
                          {:on-click (fn []
                                       (move-at! point))})
              (color-string color)]))])]
     [:div.info
      (cond winner
            [:div
             [:p (str (color-string winner) " wins!")]
             [:a {:on-click new-game!}
              "Play again"]]
            over?
            [:p "Draw game"]
            :else
            [:p (str (color-string (:player state))) "'s turn"])]]))

(defcard-rg empty
  [board empty-board])

(defcard-rg one-move
  [board (-> empty-board
             (move-at [1 1]))])

(defcard-rg three-moves
  [board (-> empty-board
             (move-at [1 1])
             (move-at [1 0])
             (move-at [2 2]))])

(deftest test-move-at
  (is (= (move-at empty-board [0 0])
         {:board {[0 0] :x}
          :player :o}))
  (is (= (-> empty-board
             (move-at [0 0])
             (move-at [1 1]))
         {:board {[0 0] :x
                  [1 1] :o}
          :player :x})))

(defcard-rg won-game
  [board {:board {[1 0] :o
                  [1 1] :o
                  [1 2] :o}}])

(defn game []
  [board @game-state])

(defcard-rg game
  [game]))
