(ns slideshow.core
  (:require
    [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]
    [sablono.core :as html :refer-macros [html]]
    [clojure.string :as string]
    [goog.events :as events]
    [goog.cssom :as cssom :refer [addCssText]]
    [garden.core :refer [css]]
    [garden.units :as u :refer [px pt]]
    [garden.color :as color :refer [hsl rgb]]
    [secretary.core :as secretary]))

(enable-console-print!)

(def habits-example {
  :chores [:na nil nil]
  :curfew [:na nil nil]
  :diet [:na nil nil]
  :getup [:success 2 10]
  :hygine [:failure nil 999]
  :improve [:na 2 nil]
  :journal [:na nil nil]
  :lift [:na nil nil]
  :network [:na nil nil]
  :plan [:na nil nil]
  :power [:na nil nil]
  :clean [:na nil nil]
  :run [:na nil nil]
  :stretch [:na nil nil]
  :weight [:na nil nil]
  :work [:na nil nil]
  })

(def app-state (atom {
  :habits habits-example
  :add-habit ""
  }))

(def stylesheet [
  [:div.toplevel { :width (px 400) :margin "0 auto" }]
  [:ul#notices { :padding (px 0) }]
  [:h1.header { :text-align :center }]
  [:.success { :background-color "#88ff88" :border-color "#228822" }]
  [:.failure { :background-color "#ff8888" :border-color "#882222" }]
  [:.na { :background-color "#ffffff" :border-color "#000000" }]
  [:ul#notices {:padding 0}]
  [:.refutable:hover { :background-color "yellow" }]
  [:input.note { :width (px 388) :border-width (px 0) }]
  [:ul.note { :padding (px 0) :list-style :none }]
  [:input.note { :background-color :inherit }]
  [:li.note:hover { :background-color :yellow }]
  [:input.note:focus { :outline 0 }]
  [:.refutable {
    :border-width (px 1)
    :border-style :solid
    :padding (px 2)
    :margin (px 1)
    :display :inline-block }]

  [:li.note {
    :padding (px 3)
    :margin (px 2)
    :display :inline-block
    :width (px 388)
    :border-width "0 2 0 2"
    :border-style :solid }]

  [:.pair-left { :margin-right 0 }]
  [:.pair-right { :margin-left -2 }]
  [:.pair-right-empty {
    :margin-left -2
    :border-color "black"
    :background-color "black" }]])

(defn add-habit [addstr owner]
  (reify om/IRender (render [_] (html
    (if (= addstr "")
      [:b "+"]
      [:input {:type "text" :value addstr}])))))

(defn note [notestr owner]
  (reify om/IRender (render [_] (html
    [:input {:type "text" :class "note" :value notestr}]))))

(defn habit [[habit-name [status chain-length habit-number]] owner]
  (reify om/IRender (render [_] (html
    [:span
      [:span {:style {:white-space "nowrap"}}
        (if (not chain-length)
          [:span {:class ["refutable" (name status) "pair-left"]}
            (name habit-name)]
          [:span {:class ["refutable" (name status) "pair-left"]}
            (name habit-name)
            [:sup (str chain-length)]])
        (if (not habit-number)
          [:sub {:class ["refutable" (name status) "pair-right-empty"]} ""]
          [:sub {:content-editable true :class ["refutable" (name status) "pair-right"]}
            (str habit-number)])]
      [:span]]))))

(defn habits [{habits-table :habits addstr :add-habit} owner]
  (reify om/IRender (render [_] (html
    [:p
      (om/build-all habit habits-table)
      (om/build add-habit addstr)]))))

(defn app [data owner]
  (reify om/IRender (render [_] (html
    [:div {:class ["toplevel"]}
      [:ul {:id "notices"}
        (om/build habits data)]
      [:ul {:class "note"}
        [:li {:class "note" :style {:border-width "2px"}}
          (om/build note "Add a Note")]
        [:li {:class "note"}
          (om/build note "hi")]]]))))

(addCssText (css stylesheet))
(om/root app app-state
  {:target (. js/document (getElementById "app"))})
