;;; pitchkai-theme.el --- A darker variant on Monokai.

;; URL: http://github.com/sjrmanning/pitchkai
;; Version: 0.1.2

;; The MIT License (MIT)

;; Copyright (c) 2017 Simon Manning

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:
;;
;; This theme and particular the structure of this file is entirely based
;; on the great work by `oneKelvinSmith' and his port of the monokai theme
;; found at https://github.com/oneKelvinSmith/monokai-emacs.
;;
;; Pitchkai arose after I found myself making more and more small tweaks
;; to monokai via a separate theme overrides file. Eventually it made more
;; sense to create a new theme as it was diverging significantly from the
;; original theme.
;;
;; Pull requests, and suggestions are most welcome!
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "The pitchkai theme requires Emacs 24 or later!"))

(deftheme pitchkai "The Pitchkai colour theme")

(defgroup pitchkai nil
  "Pitchkai theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom pitchkai-mode-line-padding 8
  "Amount of padding around the mode-line text. Set to 1 for normal look."
  :type 'number
  :group 'pitchkai)

(defcustom pitchkai-distinct-fringe-background t
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'pitchkai)

(defcustom pitchkai-use-variable-pitch t
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'pitchkai)

(defcustom pitchkai-blue-tint nil
  "Use a blue-ish tinted background rather than the flatter black."
  :type 'boolean
  :group 'pitchkai)

(defcustom pitchkai-high-contrast-mode-line nil
  "Make the active/inactive mode line stand out more."
  :type 'boolean
  :group 'pitchkai)

(defcustom pitchkai-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'pitchkai)

(defcustom pitchkai-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'pitchkai)

(defcustom pitchkai-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'pitchkai)

(defcustom pitchkai-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'pitchkai)

(defcustom pitchkai-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'pitchkai)

(defmacro pitchkai-with-color-vars (&rest body)
  (declare (indent 0))
  `(let* ((class '((class color) (min-colors 257)))
          ;; Primary colors
          (pitchkai-yellow           "#E6DB74")
          (pitchkai-orange           "#ffac4a")
          (pitchkai-red              "#ff0066")
          (pitchkai-magenta          "#ff8eff")
          (pitchkai-violet           "#ab7eff")
          (pitchkai-blue             "#06d8ff")
          (pitchkai-cyan             "#53f2dc")
          (pitchkai-green            "#63de5d")
          (pitchkai-gray             "#15191b")
          ;; Blue tints
          (pitchkai-bg-blue          "#080a16")
          (pitchkai-fringe-blue      "#323342")
          (pitchkai-pl-ld            "#38394a")
          (pitchkai-pl-d             "#323342")
          (pitchkai-pl-dd            "#090a16")
          (pitchkai-pl-l             "#424458")
          ;; Darker and lighter accented colors
          (pitchkai-yellow-d         "#BEB244")
          (pitchkai-yellow-l         "#FFF7A8")
          (pitchkai-orange-d         "#de8f2d")
          (pitchkai-orange-l         "#ffc260")
          (pitchkai-red-d            "#F70057")
          (pitchkai-red-l            "#FE61A0")
          (pitchkai-magenta-d        "#FF61FF")
          (pitchkai-magenta-l        "#FFC4FF")
          (pitchkai-violet-d         "#9257FF")
          (pitchkai-violet-l         "#C9ACFF")
          (pitchkai-blue-d           "#40CAE4")
          (pitchkai-blue-l           "#92E7F7")
          (pitchkai-cyan-d           "#00b2ac")
          (pitchkai-cyan-l           "#BBF7EF")
          (pitchkai-green-d          "#86C30D")
          (pitchkai-green-l          "#BBEF53")
          (pitchkai-gray-ld          "#181c1d")
          (pitchkai-gray-d           "#15191b")
          (pitchkai-gray-dd          "#0B0F11")
          (pitchkai-gray-l           "#515759")
          (pitchkai-green-plain      "#2AD043")
          (pitchkai-red-plain        "#FF6159")
          ;; Adaptive colors
          (pitchkai-fg               "#f8fbfc")
          (pitchkai-bg               "#000000")
          (pitchkai-highlight-line   "#323342")
          (pitchkai-highlight        "#5D6365")
          (pitchkai-emph             "#ffffff")
          (pitchkai-comments         "#6A6D70")
          ;; Adaptive higher/lower contrast accented colors
          (pitchkai-fg-hc            "#0A0A0A")
          (pitchkai-fg-lc            "#0B0C06")
          ;; High contrast colors
          (pitchkai-yellow-hc        "#FFFACE")
          (pitchkai-yellow-lc        "#9A8F21")
          (pitchkai-orange-hc        "#FFBE74")
          (pitchkai-orange-lc        "#A75B00")
          (pitchkai-red-hc           "#FEB0CC")
          (pitchkai-red-lc           "#F20055")
          (pitchkai-magenta-hc       "#FEC6F9")
          (pitchkai-magenta-lc       "#F309DF")
          (pitchkai-violet-hc        "#F0E7FF")
          (pitchkai-violet-lc        "#7830FC")
          (pitchkai-blue-hc          "#CAF5FD")
          (pitchkai-blue-lc          "#1DB4D0")
          (pitchkai-cyan-hc          "#D3FBF6")
          (pitchkai-cyan-lc          "#4BBEAE")
          (pitchkai-green-hc         "#CCF47C")
          (pitchkai-green-lc         "#63de5d")
          ;; customize based face properties
          (s-primary-bg             (if pitchkai-blue-tint
                                        pitchkai-bg-blue pitchkai-bg))
          (s-variable-pitch         (if pitchkai-use-variable-pitch
                                        'variable-pitch 'default))
          (s-distinct-fringe        (if pitchkai-blue-tint
                                        pitchkai-fringe-blue pitchkai-gray-dd))
          (s-fringe-bg              (if pitchkai-distinct-fringe-background
                                        s-distinct-fringe pitchkai-bg))
          (s-mode-line-fg           (if pitchkai-high-contrast-mode-line
                                        pitchkai-bg pitchkai-fg))
          (s-mode-line-bg           (if pitchkai-high-contrast-mode-line
                                        pitchkai-fg (if pitchkai-blue-tint
                                                        pitchkai-pl-d pitchkai-gray)))
          (s-mode-line-buffer-id-fg (if pitchkai-high-contrast-mode-line
                                        'unspecified pitchkai-green-lc))
          (s-mode-line-inactive-fg  (if pitchkai-high-contrast-mode-line
                                        pitchkai-fg pitchkai-comments))
          (s-mode-line-inactive-bg  (if pitchkai-high-contrast-mode-line
                                        pitchkai-gray-dd (if pitchkai-blue-tint
                                                             pitchkai-pl-dd pitchkai-bg)))
          (s-mode-line-inactive-bc  (if pitchkai-high-contrast-mode-line
                                        pitchkai-fg pitchkai-gray))

          ;; powerline default
          (s-powerline-default-active1-bg   (if pitchkai-high-contrast-mode-line
                                                pitchkai-gray-l pitchkai-gray))
          (s-powerline-default-active2-bg   (if pitchkai-high-contrast-mode-line
                                                pitchkai-gray pitchkai-gray-l))
          (s-powerline-default-inactive1-bg (if pitchkai-high-contrast-mode-line
                                                pitchkai-gray pitchkai-gray-d))
          (s-powerline-default-inactive2-bg (if pitchkai-high-contrast-mode-line
                                                pitchkai-bg pitchkai-gray))

          ;; powerline blue versions
          (s-powerline-active1-blue-bg   (if pitchkai-high-contrast-mode-line
                                             pitchkai-pl-l pitchkai-pl-d))
          (s-powerline-active2-blue-bg   (if pitchkai-high-contrast-mode-line
                                             pitchkai-pl-d pitchkai-pl-l))
          (s-powerline-inactive1-blue-bg (if pitchkai-high-contrast-mode-line
                                             pitchkai-pl-d pitchkai-pl-d))
          (s-powerline-inactive2-blue-bg (if pitchkai-high-contrast-mode-line
                                             pitchkai-bg pitchkai-pl-d))

          ;; powerline conditional
          (s-powerline-active1-bg   (if pitchkai-blue-tint
                                        s-powerline-active1-blue-bg s-powerline-default-active1-bg))
          (s-powerline-active2-bg   (if pitchkai-blue-tint
                                        s-powerline-active2-blue-bg s-powerline-default-active2-bg))
          (s-powerline-inactive1-bg   (if pitchkai-blue-tint
                                          s-powerline-inactive1-blue-bg s-powerline-default-inactive1-bg))
          (s-powerline-inactive2-bg   (if pitchkai-blue-tint
                                          s-powerline-inactive2-blue-bg s-powerline-default-inactive2-bg))

          ;; Definitions for terminals that do not support 256 colors
          (terminal-class                    '((class color) (min-colors 89)))
          ;; Primary colors
          (terminal-pitchkai-yellow           "#CDC673")
          (terminal-pitchkai-orange           "#FF8C00")
          (terminal-pitchkai-red              "#FF1493")
          (terminal-pitchkai-magenta          "#D700D7")
          (terminal-pitchkai-violet           "#AF87FF")
          (terminal-pitchkai-blue             "#5FD7FF")
          (terminal-pitchkai-cyan             "#5FFFFF")
          (terminal-pitchkai-green            "#87D700")
          (terminal-pitchkai-gray             "#1D1D1D")
          ;; Darker and lighter accented colors
          (terminal-pitchkai-yellow-d         "#878700")
          (terminal-pitchkai-yellow-l         "#FFFF87")
          (terminal-pitchkai-orange-d         "#AF5F00")
          (terminal-pitchkai-orange-l         "#FFAF5F")
          (terminal-pitchkai-red-d            "#870000")
          (terminal-pitchkai-red-l            "#FF5F87")
          (terminal-pitchkai-magenta-d        "#AF0087")
          (terminal-pitchkai-magenta-l        "#FF87DF")
          (terminal-pitchkai-violet-d         "#5F00AF")
          (terminal-pitchkai-violet-l         "#AF87D7")
          (terminal-pitchkai-blue-d           "#008787")
          (terminal-pitchkai-blue-l           "#87D7FF")
          (terminal-pitchkai-cyan-d           "#5FAFAF")
          (terminal-pitchkai-cyan-l           "#AFFFFF")
          (terminal-pitchkai-green-d          "#5F8700")
          (terminal-pitchkai-green-l          "#AFD700")
          (terminal-pitchkai-gray-d           "#131313")
          (terminal-pitchkai-gray-l           "#707070")
          ;; Adaptive colors
          (terminal-pitchkai-fg               "#F5F5F5")
          (terminal-pitchkai-bg               nil)
          (terminal-pitchkai-highlight-line   "#474747")
          (terminal-pitchkai-highlight        "#F4A460")
          (terminal-pitchkai-emph             "#FFFAFA")
          (terminal-pitchkai-comments         "#8B8878")
          ;; Adaptive higher/lower contrast accented colors
          (terminal-pitchkai-fg-hc            "#070A04")
          (terminal-pitchkai-fg-lc            "#0A0A0A")
          ;; High contrast colors
          (terminal-pitchkai-yellow-hc        terminal-pitchkai-yellow-d)
          (terminal-pitchkai-yellow-lc        terminal-pitchkai-yellow-l)
          (terminal-pitchkai-orange-hc        terminal-pitchkai-orange-d)
          (terminal-pitchkai-orange-lc        terminal-pitchkai-orange-l)
          (terminal-pitchkai-red-hc           terminal-pitchkai-red-d)
          (terminal-pitchkai-red-lc           terminal-pitchkai-red-l)
          (terminal-pitchkai-magenta-hc       terminal-pitchkai-magenta-d)
          (terminal-pitchkai-magenta-lc       terminal-pitchkai-magenta-l)
          (terminal-pitchkai-violet-hc        terminal-pitchkai-violet-d)
          (terminal-pitchkai-violet-lc        terminal-pitchkai-violet-l)
          (terminal-pitchkai-blue-hc          terminal-pitchkai-blue-d)
          (terminal-pitchkai-blue-lc          terminal-pitchkai-blue-l)
          (terminal-pitchkai-cyan-hc          terminal-pitchkai-cyan-d)
          (terminal-pitchkai-cyan-lc          terminal-pitchkai-cyan-l)
          (terminal-pitchkai-green-hc         terminal-pitchkai-green-d)
          (terminal-pitchkai-green-lc         terminal-pitchkai-green-l)
          ;; customize based face properties
          (terminal-s-variable-pitch         (if pitchkai-use-variable-pitch
                                                 'variable-pitch 'default))
          (terminal-s-fringe-bg              (if pitchkai-distinct-fringe-background
                                                 terminal-pitchkai-gray terminal-pitchkai-bg))
          (terminal-s-mode-line-fg           (if pitchkai-high-contrast-mode-line
                                                 terminal-pitchkai-bg terminal-pitchkai-fg))
          (terminal-s-mode-line-bg           (if pitchkai-high-contrast-mode-line
                                                 terminal-pitchkai-fg terminal-pitchkai-gray))
          (terminal-s-mode-line-buffer-id-fg (if pitchkai-high-contrast-mode-line
                                                 'unspecified terminal-pitchkai-green))
          (terminal-s-mode-line-inactive-fg  (if pitchkai-high-contrast-mode-line
                                                 terminal-pitchkai-fg terminal-pitchkai-comments))
          (terminal-s-mode-line-inactive-bg  (if pitchkai-high-contrast-mode-line
                                                 terminal-pitchkai-highlight-line terminal-pitchkai-bg))
          (terminal-s-mode-line-inactive-bc  (if pitchkai-high-contrast-mode-line
                                                 terminal-pitchkai-fg terminal-pitchkai-gray))
          )
     ,@body))

(pitchkai-with-color-vars
  ;; Define faces
  (custom-theme-set-faces
   'pitchkai

   ;; font lock for syntax highlighting
   `(font-lock-builtin-face
     ((,class (:foreground ,pitchkai-red
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(font-lock-comment-face
     ((,class (:foreground ,pitchkai-comments
                           :background nil))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(font-lock-constant-face
     ((,class (:foreground ,pitchkai-violet))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))

   `(font-lock-doc-face
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(font-lock-function-name-face
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(font-lock-keyword-face
     ((,class (:foreground ,pitchkai-red
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :weight normal))))

   `(font-lock-negation-char-face
     ((,class (:foreground ,pitchkai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight bold))))

   `(font-lock-preprocessor-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(font-lock-regexp-grouping-construct
     ((,class (:foreground ,pitchkai-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,class (:foreground ,pitchkai-violet
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-violet
                                    :weight normal))))

   `(font-lock-string-face
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(font-lock-type-face
     ((,class (:foreground ,pitchkai-blue
                           :italic nil))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :italic nil))))

   `(font-lock-variable-name-face
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(font-lock-warning-face
     ((,class (:foreground ,pitchkai-orange
                           :weight bold
                           :italic t
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-orange
                                    :weight bold
                                    :italic t
                                    :underline t))))

   `(c-annotation-face
     ((,class (:inherit font-lock-constant-face))
      (,terminal-class (:inherit font-lock-constant-face))))

   ;; general colouring
   '(button ((t (:underline t))))

   `(default
      ((,class (:foreground ,pitchkai-fg
                            :background ,s-primary-bg))
       (,terminal-class (:foreground ,terminal-pitchkai-fg
                                     :background ,terminal-pitchkai-bg))))

   `(highlight
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-highlight))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-highlight))))

   `(lazy-highlight
     ((,class (:inherit highlight
                        :background ,pitchkai-comments))
      (,terminal-class (:inherit highlight
                                 :background ,terminal-pitchkai-comments))))

   `(region
     ((,class (:inherit highlight
                        :background ,pitchkai-highlight))
      (,terminal-class (:inherit highlight
                                 :background ,terminal-pitchkai-highlight))))

   `(secondary-selection
     ((,class (:inherit region
                        :background ,pitchkai-blue))
      (,terminal-class (:inherit region
                                 :background ,terminal-pitchkai-blue))))

   `(shadow
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(match
     ((,class (:foreground ,pitchkai-cyan-l
                           :background ,pitchkai-cyan-d
                           :weight bold))
      (,terminal-class (:background ,terminal-pitchkai-cyan-l
                                    :foreground ,terminal-pitchkai-cyan-d
                                    :weight bold))))

   `(cursor
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-fg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-fg
                                    :inverse-video t))))

   `(mouse
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-fg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-fg
                                    :inverse-video t))))

   `(escape-glyph
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(escape-glyph-face
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(fringe
     ((,class (:foreground ,pitchkai-fg
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :background ,terminal-s-fringe-bg))))

   `(link
     ((,class (:foreground ,pitchkai-blue
                           :underline t
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :underline t
                                    :weight bold))))

   `(link-visited
     ((,class (:foreground ,pitchkai-violet
                           :underline t
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-violet
                                    :underline t
                                    :weight normal))))

   `(success
     ((,class (:foreground ,pitchkai-green ))
      (,terminal-class (:foreground ,terminal-pitchkai-green ))))

   `(warning
     ((,class (:foreground ,pitchkai-yellow ))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow ))))

   `(error
     ((,class (:foreground ,pitchkai-red-plain))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(eval-sexp-fu-flash
     ((,class (:foreground ,pitchkai-cyan-l
                           :background ,pitchkai-cyan-d))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan-l
                                    :background ,terminal-pitchkai-cyan-d))))

   `(eval-sexp-fu-flash-error
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-red))))

   `(trailing-whitespace
     ((,class (:background ,pitchkai-red))
      (,terminal-class (:background ,terminal-pitchkai-red))))

   `(vertical-border
     ((,class (:foreground ,pitchkai-gray))
      (,terminal-class (:foreground ,terminal-pitchkai-gray))))

   `(menu
     ((,class (:foreground ,pitchkai-fg
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :background ,terminal-pitchkai-bg))))

   `(minibuffer-prompt
     ((,class (:foreground ,pitchkai-violet-l))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))

   ;; menus and mode line
   `(mode-line
     ((,class (:inverse-video unspecified
                              :underline unspecified
                              :foreground ,s-mode-line-fg
                              :background ,s-mode-line-bg
                              :box (:line-width ,pitchkai-mode-line-padding
                                                :color ,s-mode-line-bg
                                                :style unspecified)))
      (,terminal-class (:inverse-video unspecified
                                       :underline unspecified
                                       :foreground ,terminal-s-mode-line-fg
                                       :background ,terminal-s-mode-line-bg))))

   `(mode-line-buffer-id
     ((,class (:foreground ,s-mode-line-buffer-id-fg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-s-mode-line-buffer-id-fg
                                    :weight bold))))

   `(mode-line-inactive
     ((,class (:inverse-video unspecified
                              :underline unspecified
                              :foreground ,s-mode-line-inactive-fg
                              :background ,s-mode-line-inactive-bg
                              :box (:line-width ,pitchkai-mode-line-padding
                                                :color ,s-mode-line-inactive-bg
                                                :style unspecified)))
      (,terminal-class (:inverse-video unspecified
                                       :underline unspecified
                                       :foreground ,terminal-s-mode-line-inactive-fg
                                       :background ,terminal-s-mode-line-inactive-bg))))

   `(header-line
     ((,class (:inverse-video unspecified
                              :underline unspecified
                              :foreground ,pitchkai-emph
                              :background ,pitchkai-highlight-line
                              :box (:line-width 1
                                                :color ,pitchkai-gray
                                                :style unspecified)))
      (,terminal-class (:inverse-video unspecified
                                       :underline unspecified
                                       :foreground ,terminal-pitchkai-emph
                                       :background ,terminal-pitchkai-highlight-line
                                       :box (:line-width 1
                                                         :color ,terminal-pitchkai-gray
                                                         :style unspecified)))))

   ;; cua
   `(cua-global-mark
     ((,class (:background ,pitchkai-yellow
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-yellow
                                    :foreground ,terminal-pitchkai-bg))))

   `(cua-rectangle
     ((,class (:inherit region))
      (,terminal-class (:inherit region))))

   `(cua-rectangle-noselect
     ((,class (:inherit secondary-selection))
      (,terminal-class (:inherit secondary-selection))))

   ;; diary
   `(diary
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   ;; dired
   `(dired-directory
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(dired-flagged
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(dired-header
     ((,class (:foreground ,pitchkai-blue
                           :background ,pitchkai-bg
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :background ,terminal-pitchkai-bg
                                    :inherit bold))))

   `(dired-ignored
     ((,class (:inherit shadow))
      (,terminal-class (:inherit shadow))))

   `(dired-mark
     ((,class (:foreground ,pitchkai-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :weight bold))))

   `(dired-marked
     ((,class (:foreground ,pitchkai-violet
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-pitchkai-violet
                                    :inherit bold))))

   `(dired-perm-write
     ((,class (:foreground ,pitchkai-fg
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :underline t))))

   `(dired-symlink
     ((,class (:foreground ,pitchkai-cyan
                           :slant italic))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :slant italic))))

   `(dired-warning
     ((,class (:foreground ,pitchkai-orange
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-orange
                                    :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-blue))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-blue))))

   `(dropdown-list-selection-face
     ((,class (:background ,pitchkai-green
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-green
                                    :foreground ,terminal-pitchkai-bg))))

   ;; ecb
   `(ecb-default-highlight-face
     ((,class (:background ,pitchkai-blue
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-blue
                                    :foreground ,terminal-pitchkai-bg))))

   `(ecb-history-bucket-node-dir-soure-path-face
     ((,class (:inherit ecb-history-bucket-node-face
                        :foreground ,pitchkai-yellow))
      (,terminal-class (:inherit ecb-history-bucket-node-face
                                 :foreground ,terminal-pitchkai-yellow))))

   `(ecb-source-in-directories-buffer-face
     ((,class (:inherit ecb-directories-general-face
                        :foreground ,pitchkai-fg))
      (,terminal-class (:inherit ecb-directories-general-face
                                 :foreground ,terminal-pitchkai-fg))))

   `(ecb-history-dead-buffer-face
     ((,class (:inherit ecb-history-general-face
                        :foreground ,pitchkai-comments))
      (,terminal-class (:inherit ecb-history-general-face
                                 :foreground ,terminal-pitchkai-comments))))

   `(ecb-directory-not-accessible-face
     ((,class (:inherit ecb-directories-general-face
                        :foreground ,pitchkai-comments))
      (,terminal-class (:inherit ecb-directories-general-face
                                 :foreground ,terminal-pitchkai-comments))))

   `(ecb-bucket-node-face
     ((,class (:inherit ecb-default-general-face
                        :weight normal
                        :foreground ,pitchkai-blue))
      (,terminal-class (:inherit ecb-default-general-face
                                 :weight normal
                                 :foreground ,terminal-pitchkai-blue))))

   `(ecb-tag-header-face
     ((,class (:background ,pitchkai-highlight-line))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line))))

   `(ecb-analyse-bucket-element-face
     ((,class (:inherit ecb-analyse-general-face
                        :foreground ,pitchkai-green))
      (,terminal-class (:inherit ecb-analyse-general-face
                                 :foreground ,terminal-pitchkai-green))))

   `(ecb-directories-general-face
     ((,class (:inherit ecb-default-general-face
                        :height 1.0))
      (,terminal-class (:inherit ecb-default-general-face
                                 :height 1.0))))

   `(ecb-method-non-semantic-face
     ((,class (:inherit ecb-methods-general-face
                        :foreground ,pitchkai-cyan))
      (,terminal-class (:inherit ecb-methods-general-face
                                 :foreground ,terminal-pitchkai-cyan))))

   `(ecb-mode-line-prefix-face
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(ecb-tree-guide-line-face
     ((,class (:inherit ecb-default-general-face
                        :foreground ,pitchkai-gray
                        :height 1.0))
      (,terminal-class (:inherit ecb-default-general-face
                                 :foreground ,terminal-pitchkai-gray
                                 :height 1.0))))

   ;; ee
   `(ee-bookmarked
     ((,class (:foreground ,pitchkai-emph))
      (,terminal-class (:foreground ,terminal-pitchkai-emph))))

   `(ee-category
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(ee-link
     ((,class (:inherit link))
      (,terminal-class (:inherit link))))

   `(ee-link-visited
     ((,class (:inherit link-visited))
      (,terminal-class (:inherit link-visited))))

   `(ee-marked
     ((,class (:foreground ,pitchkai-magenta
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-magenta
                                    :weight bold))))

   `(ee-omitted
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(ee-shadow
     ((,class (:inherit shadow))
      (,terminal-class (:inherit shadow))))

   ;; elixir
   `(elixir-atom-face
     ((,class (:foreground ,pitchkai-violet))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))

   `(elixir-attribute-face
     ((,class (:foreground ,pitchkai-violet-l))
      (,terminal-class (:foreground ,terminal-pitchkai-violet-l))))

   ;; grep
   `(grep-context-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(grep-error-face
     ((,class (:foreground ,pitchkai-red
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :weight bold
                                    :underline t))))

   `(grep-hit-face
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(grep-match-face
     ((,class (:foreground ,pitchkai-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :weight bold))))

   ;; isearch
   `(isearch
     ((,class (:foreground ,pitchkai-cyan-l
                           :background ,pitchkai-cyan-d))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan-l
                                    :background ,terminal-pitchkai-cyan-d))))

   `(isearch-fail
     ((,class (:inherit isearch
                        :foreground ,pitchkai-red
                        :background ,pitchkai-bg
                        :bold t))
      (,terminal-class (:inherit isearch
                                 :foreground ,terminal-pitchkai-red
                                 :background ,terminal-pitchkai-bg
                                 :bold t))))


   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,class (:foreground ,pitchkai-comments
                           :background ,pitchkai-bg
                           :inverse-video nil))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :background ,terminal-pitchkai-bg
                                    :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,class (:foreground ,pitchkai-yellow
                           :background ,pitchkai-bg
                           :inverse-video nil
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :background ,terminal-pitchkai-bg
                                    :inverse-video nil
                                    :weight bold))))

   ;; alchemist
   `(alchemist-test--failed-face
     ((,class (:foreground ,pitchkai-red-hc
                           :background ,pitchkai-red-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-red-hc
                                    :background ,terminal-pitchkai-red-lc
                                    :weight bold))))

   `(alchemist-test--success-face
     ((,class (:foreground ,pitchkai-fg
                           :background ,pitchkai-green-plain
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :background ,terminal-pitchkai-green-l
                                    :weight bold))))


   ;; auctex
   `(font-latex-bold-face
     ((,class (:inherit bold
                        :foreground ,pitchkai-emph))
      (,terminal-class (:inherit bold
                                 :foreground ,terminal-pitchkai-emph))))

   `(font-latex-doctex-documentation-face
     ((,class (:background unspecified))
      (,terminal-class (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))
      (,class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,class (:inherit italic :foreground ,pitchkai-emph))
      (,terminal-class (:inherit italic :foreground ,terminal-pitchkai-emph))))

   `(font-latex-math-face
     ((,class (:foreground ,pitchkai-violet))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))

   `(font-latex-sectioning-0-face
     ((,class (:inherit font-latex-sectioning-1-face
                        :height ,pitchkai-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-1-face
                                 :height ,pitchkai-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,class (:inherit font-latex-sectioning-2-face
                        :height ,pitchkai-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-2-face
                                 :height ,pitchkai-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,class (:inherit font-latex-sectioning-3-face
                        :height ,pitchkai-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-3-face
                                 :height ,pitchkai-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,class (:inherit font-latex-sectioning-4-face
                        :height ,pitchkai-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-4-face
                                 :height ,pitchkai-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,class (:inherit font-latex-sectioning-5-face
                        :height ,pitchkai-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-5-face
                                 :height ,pitchkai-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,pitchkai-yellow
                        :weight bold))
      (,terminal-class (:inherit ,terminal-s-variable-pitch :
                                 foreground ,terminal-pitchkai-yellow
                                 :weight bold))))

   `(font-latex-sedate-face
     ((,class (:foreground ,pitchkai-emph))
      (,terminal-class (:foreground ,terminal-pitchkai-emph))))

   `(font-latex-slide-title-face
     ((,class (:inherit (,s-variable-pitch font-lock-type-face)
                        :weight bold
                        :height ,pitchkai-height-plus-3))
      (,terminal-class (:inherit (,terminal-s-variable-pitch font-lock-type-face)
                                 :weight bold
                                 :height ,pitchkai-height-plus-3))))

   `(font-latex-string-face
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   `(font-latex-subscript-face
     ((,class (:height ,pitchkai-height-minus-1))
      (,terminal-class (:height ,pitchkai-height-minus-1))))

   `(font-latex-superscript-face
     ((,class (:height ,pitchkai-height-minus-1))
      (,terminal-class (:height ,pitchkai-height-minus-1))))

   `(font-latex-verbatim-face
     ((,class (:inherit fixed-pitch
                        :foreground ,pitchkai-fg
                        :slant italic))
      (,terminal-class (:inherit fixed-pitch
                                 :foreground ,terminal-pitchkai-fg
                                 :slant italic))))

   `(font-latex-warning-face
     ((,class (:inherit bold
                        :foreground ,pitchkai-orange))
      (,terminal-class (:inherit bold
                                 :foreground ,terminal-pitchkai-orange))))

   ;; auto-complete
   `(ac-candidate-face
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-blue))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-blue))))

   `(ac-selection-face
     ((,class (:background ,pitchkai-blue
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-blue
                                    :foreground ,terminal-pitchkai-bg))))

   `(ac-candidate-mouse-face
     ((,class (:background ,pitchkai-blue
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-blue
                                    :foreground ,terminal-pitchkai-bg))))

   `(ac-completion-face
     ((,class (:foreground ,pitchkai-emph
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-emph
                                    :underline t))))

   `(ac-gtags-candidate-face
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-blue))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-blue))))

   `(ac-gtags-selection-face
     ((,class (:background ,pitchkai-blue
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-blue
                                    :foreground ,terminal-pitchkai-bg))))

   `(ac-yasnippet-candidate-face
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-yellow))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-yellow))))

   `(ac-yasnippet-selection-face
     ((,class (:background ,pitchkai-yellow
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-yellow
                                    :foreground ,terminal-pitchkai-bg))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-blue))))

   `(ahs-edit-mode-face
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-highlight))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-highlight))))

   `(ahs-face
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-magenta
                                    :background unspecified))))

   `(ahs-plugin-bod-face
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-violet ))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-cyan ))))

   `(ahs-plugin-defalt-face
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-orange))))

   `(ahs-plugin-whole-buffer-face
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-green))))

   `(ahs-warning-face
     ((,class (:foreground ,pitchkai-red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :weight bold))))

   ;; android mode
   `(android-mode-debug-face
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(android-mode-error-face
     ((,class (:foreground ,pitchkai-orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-orange
                                    :weight bold))))

   `(android-mode-info-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(android-mode-verbose-face
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(android-mode-warning-face
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,class (:foreground ,pitchkai-violet
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-violet
                                    :weight bold))))

   `(anzu-replace-to
     ((,class (:foreground ,pitchkai-magenta-l
                           :background ,pitchkai-violet-d))
      (,terminal-class (:foreground ,terminal-pitchkai-magenta-l
                                    :background ,terminal-pitchkai-violet-d))))

   ;; bm
   `(bm-face
     ((,class (:background ,pitchkai-yellow-lc
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-yellow-lc
                                    :foreground ,terminal-pitchkai-bg))))

   `(bm-fringe-face
     ((,class (:background ,pitchkai-yellow-lc
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-yellow-lc
                                    :foreground ,terminal-pitchkai-bg))))

   `(bm-fringe-persistent-face
     ((,class (:background ,pitchkai-green-lc
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-green-lc
                                    :foreground ,terminal-pitchkai-bg))))

   `(bm-persistent-face
     ((,class (:background ,pitchkai-green-lc
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-green-lc
                                    :foreground ,terminal-pitchkai-bg))))

   ;; calfw
   `(cfw:face-day-title
     ((,class (:background ,pitchkai-highlight-line))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line))))

   `(cfw:face-annotation
     ((,class (:inherit cfw:face-day-title
                        :foreground ,pitchkai-yellow))
      (,terminal-class (:inherit cfw:face-day-title
                                 :foreground ,terminal-pitchkai-yellow))))

   `(cfw:face-default-content
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(cfw:face-default-day
     ((,class (:inherit cfw:face-day-title
                        :weight bold))
      (,terminal-class (:inherit cfw:face-day-title
                                 :weight bold))))

   `(cfw:face-disable
     ((,class (:inherit cfw:face-day-title
                        :foreground ,pitchkai-comments))
      (,terminal-class (:inherit cfw:face-day-title
                                 :foreground ,terminal-pitchkai-comments))))

   `(cfw:face-grid
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(cfw:face-header
     ((,class (:foreground ,pitchkai-blue-hc
                           :background ,pitchkai-blue-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue-hc
                                    :background ,terminal-pitchkai-blue-lc
                                    :weight bold))))

   `(cfw:face-holiday
     ((,class (:background nil
                           :foreground ,pitchkai-red
                           :weight bold))
      (,terminal-class (:background nil
                                    :foreground ,terminal-pitchkai-red
                                    :weight bold))))

   `(cfw:face-periods
     ((,class (:foreground ,pitchkai-magenta))
      (,terminal-class (:foreground ,terminal-pitchkai-magenta))))

   `(cfw:face-select
     ((,class (:background ,pitchkai-magenta-lc
                           :foreground ,pitchkai-magenta-hc))
      (,terminal-class (:background ,terminal-pitchkai-magenta-lc
                                    :foreground ,terminal-pitchkai-magenta-hc))))

   `(cfw:face-saturday
     ((,class (:foreground ,pitchkai-cyan-hc
                           :background ,pitchkai-cyan-lc))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan-hc
                                    :background ,terminal-pitchkai-cyan-lc))))

   `(cfw:face-sunday
     ((,class (:foreground ,pitchkai-red-hc
                           :background ,pitchkai-red-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-red-hc
                                    :background ,terminal-pitchkai-red-lc
                                    :weight bold))))

   `(cfw:face-title
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,pitchkai-yellow
                        :weight bold
                        :height ,pitchkai-height-plus-4))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-pitchkai-yellow
                                 :weight bold
                                 :height ,pitchkai-height-plus-4))))

   `(cfw:face-today
     ((,class (:weight bold
                       :background ,pitchkai-highlight-line
                       :foreground nil))
      (,terminal-class (:weight bold
                                :background ,terminal-pitchkai-highlight-line
                                :foreground nil))))

   `(cfw:face-today-title
     ((,class (:background ,pitchkai-yellow-lc
                           :foreground ,pitchkai-yellow-hc
                           :weight bold))
      (,terminal-class (:background ,terminal-pitchkai-yellow-lc
                                    :foreground ,terminal-pitchkai-yellow-hc
                                    :weight bold))))

   `(cfw:face-toolbar
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-fg))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-fg))))

   `(cfw:face-toolbar-button-off
     ((,class (:background ,pitchkai-yellow-lc
                           :foreground ,pitchkai-yellow-hc
                           :weight bold))
      (,terminal-class (:background ,terminal-pitchkai-yellow-lc
                                    :foreground ,terminal-pitchkai-yellow-hc
                                    :weight bold))))

   `(cfw:face-toolbar-button-on
     ((,class (:background ,pitchkai-yellow-hc
                           :foreground ,pitchkai-yellow-lc
                           :weight bold))
      (,terminal-class (:background ,terminal-pitchkai-yellow-hc
                                    :foreground ,terminal-pitchkai-yellow-lc
                                    :weight bold))))

   ;; cider
   `(cider-enlightened
     ((,class (:foreground ,pitchkai-yellow
                           :background nil
                           :box (:color ,pitchkai-yellow :line-width -1 :style nil)))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :background nil
                                    :box (:color ,terminal-pitchkai-yellow :line-width -1 :style nil))) ))

   `(cider-enlightened-local
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(cider-instrumented-face
     ((,class (:foreground ,pitchkai-violet
                           :background nil
                           :box (:color ,pitchkai-violet :line-width -1 :style nil)))
      (,terminal-class (:foreground ,terminal-pitchkai-violet
                                    :background nil
                                    :box (:color ,terminal-pitchkai-violet :line-width -1 :style nil)))))

   `(cider-result-overlay-face
     ((,class (:foreground ,pitchkai-blue
                           :background nil
                           :box (:color ,pitchkai-blue :line-width -1 :style nil)))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :background nil
                                    :box (:color ,terminal-pitchkai-blue :line-width -1 :style nil)))))

   `(cider-test-error-face
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-orange))))

   `(cider-test-failure-face
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-red))))

   `(cider-test-success-face
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-green))))

   `(cider-traced-face
     ((,class :box (:color ,pitchkai-blue :line-width -1 :style nil))
      (,terminal-class :box (:color ,terminal-pitchkai-blue :line-width -1 :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face
     ((,class (:foreground ,pitchkai-red
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :weight bold
                                    :underline t))))

   `(clojure-test-error-face
     ((,class (:foreground ,pitchkai-orange
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :weight bold
                                    :underline t))))

   `(clojure-test-success-face
     ((,class (:foreground ,pitchkai-green
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :weight bold
                                    :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-emph))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-emph))))

   `(company-tooltip-selection
     ((,class (:background ,pitchkai-cyan-d
                           :foreground ,pitchkai-cyan-l))
      (,terminal-class (:background ,terminal-pitchkai-cyan-d
                                    :foreground ,terminal-pitchkai-cyan-l))))

   `(company-tooltip-mouse
     ((,class (:background ,pitchkai-blue
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-blue
                                    :foreground ,terminal-pitchkai-bg))))

   `(company-tooltip-common
     ((,class (:foreground ,pitchkai-violet
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-violet
                                    :underline t))))

   `(company-tooltip-common-selection
     ((,class (:foreground ,pitchkai-fg
                           :background ,pitchkai-cyan-d
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :background ,terminal-pitchkai-cyan-d
                                    :underline t))))

   `(company-preview
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-emph))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-emph))))

   `(company-preview-common
     ((,class (:foreground ,pitchkai-blue
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :underline t))))

   `(company-scrollbar-bg
     ((,class (:background ,pitchkai-gray))
      (,terminal-class (:background ,terminal-pitchkai-gray))))

   `(company-scrollbar-fg
     ((,class (:background ,pitchkai-comments))
      (,terminal-class (:background ,terminal-pitchkai-comments))))

   `(company-tooltip-annotation
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(company-template-field
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-blue))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-blue))))

   ;; compilation
   `(compilation-column-face
     ((,class (:foreground ,pitchkai-cyan
                           :underline nil))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :underline nil))))

   `(compilation-column-number
     ((,class (:inherit font-lock-doc-face
                        :foreground ,pitchkai-cyan
                        :underline nil))
      (,terminal-class (:inherit font-lock-doc-face
                                 :foreground ,terminal-pitchkai-cyan
                                 :underline nil))))

   `(compilation-enter-directory-face
     ((,class (:foreground ,pitchkai-green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :underline nil))))

   `(compilation-error
     ((,class (:inherit error
                        :underline nil))
      (,terminal-class (:inherit error
                                 :underline nil))))

   `(compilation-error-face
     ((,class (:foreground ,pitchkai-red
                           :underline nil))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :underline nil))))

   `(compilation-face
     ((,class (:foreground ,pitchkai-fg
                           :underline nil))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :underline nil))))

   `(compilation-info
     ((,class (:foreground ,pitchkai-comments
                           :underline nil
                           :bold nil))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :underline nil
                                    :bold nil))))

   `(compilation-info-face
     ((,class (:foreground ,pitchkai-blue
                           :underline nil))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :underline nil))))

   `(compilation-leave-directory-face
     ((,class (:foreground ,pitchkai-green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :underline nil))))

   `(compilation-line-face
     ((,class (:foreground ,pitchkai-green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :underline nil))))

   `(compilation-line-number
     ((,class (:foreground ,pitchkai-green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :underline nil))))

   `(compilation-warning
     ((,class (:inherit warning
                        :underline nil))
      (,terminal-class (:inherit warning
                                 :underline nil))))

   `(compilation-warning-face
     ((,class (:foreground ,pitchkai-yellow
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight normal
                                    :underline nil))))

   `(compilation-mode-line-exit
     ((,class (:inherit compilation-info
                        :foreground ,pitchkai-green
                        :weight bold))
      (,terminal-class (:inherit compilation-info
                                 :foreground ,terminal-pitchkai-green
                                 :weight bold))))

   `(compilation-mode-line-fail
     ((,class (:inherit compilation-error
                        :foreground ,pitchkai-red
                        :weight bold))
      (,terminal-class (:inherit compilation-error
                                 :foreground ,terminal-pitchkai-red
                                 :weight bold))))

   `(compilation-mode-line-run
     ((,class (:foreground ,pitchkai-orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-orange
                                    :weight bold))))

   ;; CSCOPE
   `(cscope-file-face
     ((,class (:foreground ,pitchkai-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :weight bold))))

   `(cscope-function-face
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(cscope-line-number-face
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(cscope-line-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(cscope-mouse-face
     ((,class (:background ,pitchkai-blue
                           :foreground ,pitchkai-fg))
      (,terminal-class (:background ,terminal-pitchkai-blue
                                    :foreground ,terminal-pitchkai-fg))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-emph
                           :underline ,pitchkai-emph
                           :weight bold))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-emph
                                    :underline ,terminal-pitchkai-emph
                                    :weight bold))))

   `(ctbl:face-continue-bar
     ((,class (:background ,pitchkai-gray
                           :foreground ,pitchkai-yellow))
      (,terminal-class (:background ,terminal-pitchkai-gray
                                    :foreground ,terminal-pitchkai-yellow))))

   `(ctbl:face-row-select
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-fg
                           :underline t))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-fg
                                    :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,class (:foreground ,pitchkai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight bold))))

   `(coffee-mode-function-param
     ((,class (:foreground ,pitchkai-violet
                           :slant italic))
      (,terminal-class (:foreground ,terminal-pitchkai-violet
                                    :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,class (:inherit ,s-variable-pitch
                        :height ,pitchkai-height-plus-3
                        :foreground ,pitchkai-violet
                        :weight bold))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :height ,pitchkai-height-plus-3
                                 :foreground ,terminal-pitchkai-violet
                                 :weight bold))))

   `(custom-variable-tag
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,pitchkai-cyan
                        :height ,pitchkai-height-plus-3))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-pitchkai-cyan
                                 :height ,pitchkai-height-plus-3))))

   `(custom-comment-tag
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(custom-group-tag
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,pitchkai-blue
                        :height ,pitchkai-height-plus-3))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-pitchkai-blue
                                 :height ,pitchkai-height-plus-3))))

   `(custom-group-tag-1
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,pitchkai-red
                        :height ,pitchkai-height-plus-3))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-pitchkai-red
                                 :height ,pitchkai-height-plus-3))))

   `(custom-state
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   ;; diff
   `(diff-added
     ((,class (:foreground ,pitchkai-green-plain
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :background ,terminal-pitchkai-bg))))

   `(diff-changed
     ((,class (:foreground ,pitchkai-blue
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :background ,terminal-pitchkai-bg))))

   `(diff-removed
     ((,class (:foreground ,pitchkai-red-plain
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :background ,terminal-pitchkai-bg))))

   `(diff-header
     ((,class (:background ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-bg))))

   `(diff-file-header
     ((,class (:background ,pitchkai-bg
                           :foreground ,pitchkai-fg
                           :weight bold))
      (,terminal-class (:background ,terminal-pitchkai-bg
                                    :foreground ,terminal-pitchkai-fg
                                    :weight bold))))

   `(diff-refine-added
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-green-plain))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-green))))

   `(diff-refine-change
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-blue))))

   `(diff-refine-removed
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-red-plain))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-red))))

   ;; diff-hl
   `(diff-hl-change
     ((,class (:background ,pitchkai-blue-lc
                           :foreground ,pitchkai-blue-hc))
      (,terminal-class (:background ,terminal-pitchkai-blue-lc
                                    :foreground ,terminal-pitchkai-blue-hc))))

   `(diff-hl-delete
     ((,class (:background ,pitchkai-red-lc
                           :foreground ,pitchkai-red-hc))
      (,terminal-class (:background ,terminal-pitchkai-red-lc
                                    :foreground ,terminal-pitchkai-red-hc))))

   `(diff-hl-insert
     ((,class (:background ,pitchkai-green-lc
                           :foreground ,pitchkai-green-hc))
      (,terminal-class (:background ,terminal-pitchkai-green-lc
                                    :foreground ,terminal-pitchkai-green-hc))))

   `(diff-hl-unknown
     ((,class (:background ,pitchkai-violet-lc
                           :foreground ,pitchkai-violet-hc))
      (,terminal-class (:background ,terminal-pitchkai-violet-lc
                                    :foreground ,terminal-pitchkai-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,class (:background ,pitchkai-orange-lc))
      (,terminal-class (:background ,terminal-pitchkai-orange-lc))))

   `(ediff-fine-diff-B
     ((,class (:background ,pitchkai-green-lc))
      (,terminal-class (:background ,terminal-pitchkai-green-lc))))

   `(ediff-fine-diff-C
     ((,class (:background ,pitchkai-yellow-lc))
      (,terminal-class (:background ,terminal-pitchkai-yellow-lc))))

   `(ediff-current-diff-C
     ((,class (:background ,pitchkai-blue-lc))
      (,terminal-class (:background ,terminal-pitchkai-blue-lc))))

   `(ediff-even-diff-A
     ((,class (:background ,pitchkai-comments
                           :foreground ,pitchkai-fg-lc ))
      (,terminal-class (:background ,terminal-pitchkai-comments
                                    :foreground ,terminal-pitchkai-fg-lc ))))

   `(ediff-odd-diff-A
     ((,class (:background ,pitchkai-comments
                           :foreground ,pitchkai-fg-hc ))
      (,terminal-class (:background ,terminal-pitchkai-comments
                                    :foreground ,terminal-pitchkai-fg-hc ))))

   `(ediff-even-diff-B
     ((,class (:background ,pitchkai-comments
                           :foreground ,pitchkai-fg-hc ))
      (,terminal-class (:background ,terminal-pitchkai-comments
                                    :foreground ,terminal-pitchkai-fg-hc ))))

   `(ediff-odd-diff-B
     ((,class (:background ,pitchkai-comments
                           :foreground ,pitchkai-fg-lc ))
      (,terminal-class (:background ,terminal-pitchkai-comments
                                    :foreground ,terminal-pitchkai-fg-lc ))))

   `(ediff-even-diff-C
     ((,class (:background ,pitchkai-comments
                           :foreground ,pitchkai-fg ))
      (,terminal-class (:background ,terminal-pitchkai-comments
                                    :foreground ,terminal-pitchkai-fg ))))

   `(ediff-odd-diff-C
     ((,class (:background ,pitchkai-comments
                           :foreground ,pitchkai-bg ))
      (,terminal-class (:background ,terminal-pitchkai-comments
                                    :foreground ,terminal-pitchkai-bg ))))

   ;; edts
   `(edts-face-error-line
     ((,(append '((supports :underline (:style line))) class)
       (:underline (:style line :color ,pitchkai-red)
                   :inherit unspecified))
      (,class (:foreground ,pitchkai-red-hc
                           :background ,pitchkai-red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style line))) terminal-class)
       (:underline (:style line :color ,terminal-pitchkai-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-pitchkai-red-hc
                                    :background ,terminal-pitchkai-red-lc
                                    :weight bold
                                    :underline t))))

   `(edts-face-warning-line
     ((,(append '((supports :underline (:style line))) class)
       (:underline (:style line :color ,pitchkai-yellow)
                   :inherit unspecified))
      (,class (:foreground ,pitchkai-yellow-hc
                           :background ,pitchkai-yellow-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style line))) terminal-class)
       (:underline (:style line :color ,terminal-pitchkai-yellow)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow-hc
                                    :background ,terminal-pitchkai-yellow-lc
                                    :weight bold
                                    :underline t))))

   `(edts-face-error-fringe-bitmap
     ((,class (:foreground ,pitchkai-red
                           :background unspecified
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :background unspecified
                                    :weight bold))))

   `(edts-face-warning-fringe-bitmap
     ((,class (:foreground ,pitchkai-yellow
                           :background unspecified
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :background unspecified
                                    :weight bold))))

   `(edts-face-error-mode-line
     ((,class (:background ,pitchkai-red
                           :foreground unspecified))
      (,terminal-class (:background ,terminal-pitchkai-red
                                    :foreground unspecified))))

   `(edts-face-warning-mode-line
     ((,class (:background ,pitchkai-yellow
                           :foreground unspecified))
      (,terminal-class (:background ,terminal-pitchkai-yellow
                                    :foreground unspecified))))


   ;; elfeed
   `(elfeed-search-date-face
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(elfeed-search-feed-face
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(elfeed-search-tag-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(elfeed-search-title-face
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   ;; ein
   `(ein:cell-input-area
     ((,class (:background ,pitchkai-highlight-line))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line))))
   `(ein:cell-input-prompt
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))
   `(ein:cell-output-prompt
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))
   `(ein:notification-tab-normal
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))
   `(ein:notification-tab-selected
     ((,class (:foreground ,pitchkai-orange :inherit bold))
      (,terminal-class (:foreground ,terminal-pitchkai-orange :inherit bold))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,class (:inherit font-lock-keyword-face))
      (,terminal-class (:inherit font-lock-keyword-face))))

   ;; erm-syn
   `(erm-syn-errline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,pitchkai-red)
                   :inherit unspecified))
      (,class (:foreground ,pitchkai-red-hc
                           :background ,pitchkai-red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-pitchkai-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-pitchkai-red-hc
                                    :background ,terminal-pitchkai-red-lc
                                    :weight bold
                                    :underline t))))

   `(erm-syn-warnline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,pitchkai-orange)
                   :inherit unspecified))
      (,class (:foreground ,pitchkai-orange-hc
                           :background ,pitchkai-orange-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-pitchkai-orange)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-pitchkai-orange-hc
                                    :background ,terminal-pitchkai-orange-lc
                                    :weight bold
                                    :underline t))))

   ;; epc
   `(epc:face-title
     ((,class (:foreground ,pitchkai-blue
                           :background ,pitchkai-bg
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :background ,terminal-pitchkai-bg
                                    :weight normal
                                    :underline nil))))

   ;; erc
   `(erc-action-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-bold-face
     ((,class (:weight bold))
      (,terminal-class (:weight bold))))

   `(erc-current-nick-face
     ((,class (:foreground ,pitchkai-blue :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :weight bold))))

   `(erc-dangerous-host-face
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(erc-default-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(erc-highlight-face
     ((,class (:inherit erc-default-face
                        :background ,pitchkai-highlight))
      (,terminal-class (:inherit erc-default-face
                                 :background ,terminal-pitchkai-highlight))))

   `(erc-direct-msg-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-error-face
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(erc-fool-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-input-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-keyword-face
     ((,class (:foreground ,pitchkai-blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :weight bold))))

   `(erc-nick-default-face
     ((,class (:foreground ,pitchkai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight bold))))

   `(erc-my-nick-face
     ((,class (:foreground ,pitchkai-red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :weight bold))))

   `(erc-nick-msg-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-notice-face
     ((,class (:inherits erc-default-face))
      (,terminal-class (:inherits erc-default-face))))

   `(erc-pal-face
     ((,class (:foreground ,pitchkai-orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-orange
                                    :weight bold))))

   `(erc-prompt-face
     ((,class (:foreground ,pitchkai-blue
                           :background ,pitchkai-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :background ,terminal-pitchkai-bg
                                    :weight bold))))

   `(erc-timestamp-face
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(erc-underline-face
     ((t (:underline t))))

   ;; eshell
   `(eshell-prompt
     ((,class (:foreground ,pitchkai-blue
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :inherit bold))))

   `(eshell-ls-archive
     ((,class (:foreground ,pitchkai-red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :inherit bold))))

   `(eshell-ls-backup
     ((,class (:inherit font-lock-comment-face))
      (,terminal-class (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,class (:inherit font-lock-comment-face))
      (,terminal-class (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,class (:foreground ,pitchkai-blue
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :inherit bold))))

   `(eshell-ls-executable
     ((,class (:foreground ,pitchkai-green
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :inherit bold))))

   `(eshell-ls-unreadable
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(eshell-ls-missing
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,class (:inherit font-lock-doc-face))
      (,terminal-class (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,class (:foreground ,pitchkai-yellow
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :inherit bold))))

   `(eshell-ls-symlink
     ((,class (:foreground ,pitchkai-cyan
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :inherit bold))))

   ;; evil-ex-substitute
   `(evil-ex-substitute-matches
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-red-l
                           :inherit italic))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-red-l
                                    :inherit italic))))
   `(evil-ex-substitute-replacement
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-green-l
                           :inherit italic))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line :foreground ,terminal-pitchkai-green-l :inherit italic))))

   ;; evil-search-highlight-persist
   `(evil-search-highlight-persist-highlight-face
     ((,class (:inherit region))
      (,terminal-class (:inherit region))))

   ;; fic
   `(fic-author-face
     ((,class (:background ,pitchkai-bg
                           :foreground ,pitchkai-orange
                           :underline t
                           :slant italic))
      (,terminal-class (:background ,terminal-pitchkai-bg
                                    :foreground ,terminal-pitchkai-orange
                                    :underline t
                                    :slant italic))))

   `(fic-face
     ((,class (:background ,pitchkai-bg
                           :foreground ,pitchkai-orange
                           :weight normal
                           :slant italic))
      (,terminal-class (:background ,terminal-pitchkai-bg
                                    :foreground ,terminal-pitchkai-orange
                                    :weight normal
                                    :slant italic))))

   `(font-lock-fic-face
     ((,class (:background ,pitchkai-bg
                           :foreground ,pitchkai-orange
                           :weight normal
                           :slant italic))
      (,terminal-class (:background ,terminal-pitchkai-bg
                                    :foreground ,terminal-pitchkai-orange
                                    :weight normal
                                    :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,class (:foreground ,pitchkai-cyan
                           :weight normal
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :weight normal
                                    :underline t))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,pitchkai-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,class (:foreground ,pitchkai-red-hc
                           :background ,pitchkai-red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-pitchkai-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,terminal-class (:foreground ,terminal-pitchkai-red-hc
                                    :background ,terminal-pitchkai-red-lc
                                    :weight bold
                                    :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,pitchkai-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,class (:foreground ,pitchkai-green-hc
                           :background ,pitchkai-green-lc))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-pitchkai-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,terminal-class (:foreground ,terminal-pitchkai-green-hc
                                    :background ,terminal-pitchkai-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,pitchkai-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,class (:foreground ,pitchkai-yellow-hc
                           :background ,pitchkai-yellow-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-pitchkai-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow-hc
                                    :background ,terminal-pitchkai-yellow-lc
                                    :weight bold
                                    :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,pitchkai-red)
                   :inherit unspecified))
      (,class (:foreground ,pitchkai-red-hc
                           :background ,pitchkai-red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-pitchkai-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-pitchkai-red-hc
                                    :background ,terminal-pitchkai-red-lc
                                    :weight bold
                                    :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,pitchkai-yellow)
                   :inherit unspecified))
      (,class (:foreground ,pitchkai-yellow-hc
                           :background ,pitchkai-yellow-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-pitchkai-yellow)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow-hc
                                    :background ,terminal-pitchkai-yellow-lc
                                    :weight bold
                                    :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,pitchkai-blue)
                   :inherit unspecified))
      (,class (:foreground ,pitchkai-blue-hc
                           :background ,pitchkai-blue-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-pitchkai-blue)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-pitchkai-blue-hc
                                    :background ,terminal-pitchkai-blue-lc
                                    :weight bold
                                    :underline t))))

   `(flycheck-fringe-error
     ((,class (:foreground ,pitchkai-red-hc
                           :background ,pitchkai-red-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-red-hc
                                    :background ,terminal-pitchkai-red-lc
                                    :weight bold))))

   `(flycheck-fringe-warning
     ((,class (:foreground ,pitchkai-yellow-hc
                           :background ,pitchkai-yellow-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow-hc
                                    :background ,terminal-pitchkai-yellow-lc
                                    :weight bold))))

   `(flycheck-fringe-info
     ((,class (:foreground ,pitchkai-blue-hc
                           :background ,pitchkai-blue-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue-hc
                                    :background ,terminal-pitchkai-blue-lc
                                    :weight bold))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,pitchkai-yellow)
                   :inherit unspecified))
      (,class (:foreground ,pitchkai-yellow
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-pitchkai-yellow)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight bold
                                    :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,pitchkai-red)
                   :inherit unspecified))
      (,class (:foreground ,pitchkai-red
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-pitchkai-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :weight bold
                                    :underline t))))


   ;; git-gutter
   `(git-gutter:added
     ((,class (:background ,pitchkai-green
                           :foreground ,pitchkai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-pitchkai-green
                                    :foreground ,terminal-pitchkai-bg
                                    :inherit bold))))

   `(git-gutter:deleted
     ((,class (:background ,pitchkai-red
                           :foreground ,pitchkai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-pitchkai-red
                                    :foreground ,terminal-pitchkai-bg
                                    :inherit bold))))

   `(git-gutter:modified
     ((,class (:background ,pitchkai-blue
                           :foreground ,pitchkai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-pitchkai-blue
                                    :foreground ,terminal-pitchkai-bg
                                    :inherit bold))))

   `(git-gutter:unchanged
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-bg
                                    :inherit bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,class (:foreground ,pitchkai-green
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :inherit bold))))

   `(git-gutter-fr:deleted
     ((,class (:foreground ,pitchkai-red
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :inherit bold))))

   `(git-gutter-fr:modified
     ((,class (:foreground ,pitchkai-blue
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :inherit bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,class (:background ,pitchkai-green
                           :foreground ,pitchkai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-pitchkai-green
                                    :foreground ,terminal-pitchkai-bg
                                    :inherit bold))))

   `(git-gutter+-deleted
     ((,class (:background ,pitchkai-red
                           :foreground ,pitchkai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-pitchkai-red
                                    :foreground ,terminal-pitchkai-bg
                                    :inherit bold))))

   `(git-gutter+-modified
     ((,class (:background ,pitchkai-blue
                           :foreground ,pitchkai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-pitchkai-blue
                                    :foreground ,terminal-pitchkai-bg
                                    :inherit bold))))

   `(git-gutter+-unchanged
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-bg
                                    :inherit bold))))

   `(git-gutter-fr+-added
     ((,class (:foreground ,pitchkai-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :weight bold))))

   `(git-gutter-fr+-deleted
     ((,class (:foreground ,pitchkai-red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :weight bold))))

   `(git-gutter-fr+-modified
     ((,class (:foreground ,pitchkai-blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :weight bold))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face
     ((,class (:foreground ,pitchkai-blue
                           :background ,pitchkai-highlight-line
                           :inherit bold))
      (,terminal-class (:foreground ,pitchkai-blue
                                    :background ,terminal-pitchkai-highlight-line
                                    :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(guide-key/key-face
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(guide-key/prefix-command-face
     ((,class (:foreground ,pitchkai-violet))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))

   ;; gnus
   `(gnus-group-mail-1
     ((,class (:weight bold
                       :inherit gnus-group-mail-1-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-1-empty))))

   `(gnus-group-mail-1-empty
     ((,class (:inherit gnus-group-news-1-empty))
      (,terminal-class (:inherit gnus-group-news-1-empty))))

   `(gnus-group-mail-2
     ((,class (:weight bold
                       :inherit gnus-group-mail-2-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-2-empty))))

   `(gnus-group-mail-2-empty
     ((,class (:inherit gnus-group-news-2-empty))
      (,terminal-class (:inherit gnus-group-news-2-empty))))

   `(gnus-group-mail-3
     ((,class (:weight bold
                       :inherit gnus-group-mail-3-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-3-empty))))

   `(gnus-group-mail-3-empty
     ((,class (:inherit gnus-group-news-3-empty))
      (,terminal-class (:inherit gnus-group-news-3-empty))))

   `(gnus-group-mail-low
     ((,class (:weight bold
                       :inherit gnus-group-mail-low-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-low-empty))))

   `(gnus-group-mail-low-empty
     ((,class (:inherit gnus-group-news-low-empty))
      (,terminal-class (:inherit gnus-group-news-low-empty))))

   `(gnus-group-news-1
     ((,class (:weight bold
                       :inherit gnus-group-news-1-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-1-empty))))

   `(gnus-group-news-2
     ((,class (:weight bold
                       :inherit gnus-group-news-2-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-2-empty))))

   `(gnus-group-news-3
     ((,class (:weight bold
                       :inherit gnus-group-news-3-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-3-empty))))

   `(gnus-group-news-4
     ((,class (:weight bold
                       :inherit gnus-group-news-4-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-4-empty))))

   `(gnus-group-news-5
     ((,class (:weight bold
                       :inherit gnus-group-news-5-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-5-empty))))

   `(gnus-group-news-6
     ((,class (:weight bold
                       :inherit gnus-group-news-6-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-6-empty))))

   `(gnus-group-news-low
     ((,class (:weight bold
                       :inherit gnus-group-news-low-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-low-empty))))

   `(gnus-header-content
     ((,class (:inherit message-header-other))
      (,terminal-class (:inherit message-header-other))))

   `(gnus-header-from
     ((,class (:inherit message-header-other))
      (,terminal-class (:inherit message-header-other))))

   `(gnus-header-name
     ((,class (:inherit message-header-name))
      (,terminal-class (:inherit message-header-name))))

   `(gnus-header-newsgroups
     ((,class (:inherit message-header-other))
      (,terminal-class (:inherit message-header-other))))

   `(gnus-header-subject
     ((,class (:inherit message-header-subject))
      (,terminal-class (:inherit message-header-subject))))

   `(gnus-summary-cancelled
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(gnus-summary-high-ancient
     ((,class (:foreground ,pitchkai-blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :weight bold))))

   `(gnus-summary-high-read
     ((,class (:foreground ,pitchkai-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :weight bold))))

   `(gnus-summary-high-ticked
     ((,class (:foreground ,pitchkai-orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-orange
                                    :weight bold))))

   `(gnus-summary-high-unread
     ((,class (:foreground ,pitchkai-fg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :weight bold))))

   `(gnus-summary-low-ancient
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(gnus-summary-low-read
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(gnus-summary-low-ticked
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(gnus-summary-low-unread
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(gnus-summary-normal-ancient
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(gnus-summary-normal-read
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(gnus-summary-normal-ticked
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(gnus-summary-normal-unread
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(gnus-summary-selected
     ((,class (:foreground ,pitchkai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight bold))))

   `(gnus-cite-1
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(gnus-cite-2
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(gnus-cite-3
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(gnus-cite-4
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(gnus-cite-5
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(gnus-cite-6
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(gnus-cite-7
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(gnus-cite-8
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(gnus-cite-9
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(gnus-cite-10
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(gnus-cite-11
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(gnus-group-news-1-empty
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(gnus-group-news-2-empty
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(gnus-group-news-3-empty
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(gnus-group-news-4-empty
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(gnus-group-news-5-empty
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(gnus-group-news-6-empty
     ((,class (:foreground ,pitchkai-blue-lc))
      (,terminal-class (:foreground ,terminal-pitchkai-blue-lc))))

   `(gnus-group-news-low-empty
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(gnus-signature
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(gnus-x-face
     ((,class (:background ,pitchkai-fg
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-fg
                                    :foreground ,terminal-pitchkai-bg))))


   ;; helm
   `(helm-apt-deinstalled
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(helm-apt-installed
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(helm-bookmark-directory
     ((,class (:inherit helm-ff-directory))
      (,terminal-class (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(helm-bookmark-gnus
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   `(helm-bookmark-info
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(helm-bookmark-man
     ((,class (:foreground ,pitchkai-violet))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))

   `(helm-bookmark-w3m
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(helm-bookmarks-su
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(helm-buffer-file
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(helm-buffer-directory
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(helm-buffer-process
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(helm-buffer-saved-out
     ((,class (:foreground ,pitchkai-red
                           :background ,pitchkai-bg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :background ,terminal-pitchkai-bg
                                    :inverse-video t))))

   `(helm-buffer-size
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(helm-candidate-number
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-emph
                           :bold t))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-emph
                                    :bold t))))

   `(helm-ff-directory
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(helm-ff-executable
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(helm-ff-file
     ((,class (:background ,pitchkai-bg
                           :foreground ,pitchkai-fg))
      (,terminal-class (:background ,terminal-pitchkai-bg
                                    :foreground ,terminal-pitchkai-fg))))

   `(helm-ff-invalid-symlink
     ((,class (:background ,pitchkai-bg
                           :foreground ,pitchkai-orange
                           :slant italic))
      (,terminal-class (:background ,terminal-pitchkai-bg
                                    :foreground ,terminal-pitchkai-orange
                                    :slant italic))))

   `(helm-ff-prefix
     ((,class (:background ,pitchkai-green
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-green
                                    :foreground ,terminal-pitchkai-bg))))

   `(helm-ff-symlink
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   `(helm-grep-file
     ((,class (:foreground ,pitchkai-cyan
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :underline t))))

   `(helm-grep-finish
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(helm-grep-lineno
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(helm-grep-match
     ((,class (:inherit helm-match)))
     ((,terminal-class (:inherit helm-match))))

   `(helm-grep-running
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(helm-header
     ((,class (:inherit header-line))
      (,terminal-class (:inherit terminal-header-line))))

   `(helm-lisp-completion-info
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(helm-lisp-show-completion
     ((,class (:foreground ,pitchkai-yellow
                           :background ,pitchkai-highlight-line
                           :bold t))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :background ,terminal-pitchkai-highlight-line
                                    :bold t))))

   `(helm-M-x-key
     ((,class (:foreground ,pitchkai-orange
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-orange
                                    :underline t))))

   `(helm-moccur-buffer
     ((,class (:foreground ,pitchkai-cyan
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :underline t))))

   `(helm-match
     ((,class (:foreground ,pitchkai-green :inherit bold))
      (,terminal-class (:foreground ,terminal-pitchkai-green :inherit bold))))

   `(helm-match-item
     ((,class (:inherit helm-match))
      (,terminal-class (:inherit helm-match))))

   `(helm-selection
     ((,class (:background ,pitchkai-highlight-line
                           :inherit bold
                           :underline nil))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :inherit bold
                                    :underline nil))))

   `(helm-selection-line
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-emph
                           :underline nil))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-emph
                                    :underline nil))))

   `(helm-separator
     ((,class (:foreground ,pitchkai-gray))
      (,terminal-class (:foreground ,terminal-pitchkai-gray))))

   `(helm-source-header
     ((,class (:background ,pitchkai-violet-l
                           :foreground ,pitchkai-bg
                           :underline nil))
      (,terminal-class (:background ,terminal-pitchkai-violet-l
                                    :foreground ,terminal-pitchkai-bg
                                    :underline nil))))

   `(helm-swoop-target-line-face
     ((,class (:background ,pitchkai-highlight-line))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line))))

   `(helm-swoop-target-line-block-face
     ((,class (:background ,pitchkai-highlight-line))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line))))

   `(helm-swoop-target-word-face
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(helm-time-zone-current
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(helm-time-zone-home
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(helm-visible-mark
     ((,class (:background ,pitchkai-bg
                           :foreground ,pitchkai-magenta :bold t))
      (,terminal-class (:background ,terminal-pitchkai-bg
                                    :foreground ,terminal-pitchkai-magenta :bold t))))

   ;; helm-ls-git
   `(helm-ls-git-modified-not-staged-face
     ((,class :foreground ,pitchkai-blue)
      (,terminal-class :foreground ,terminal-pitchkai-blue)))

   `(helm-ls-git-modified-and-staged-face
     ((,class :foreground ,pitchkai-blue-l)
      (,terminal-class :foreground ,terminal-pitchkai-blue-l)))

   `(helm-ls-git-renamed-modified-face
     ((,class :foreground ,pitchkai-blue-l)
      (,terminal-class :foreground ,terminal-pitchkai-blue-l)))

   `(helm-ls-git-untracked-face
     ((,class :foreground ,pitchkai-orange)
      (,terminal-class :foreground ,terminal-pitchkai-orange)))

   `(helm-ls-git-added-copied-face
     ((,class :foreground ,pitchkai-green)
      (,terminal-class :foreground ,terminal-pitchkai-green)))

   `(helm-ls-git-added-modified-face
     ((,class :foreground ,pitchkai-green-l)
      (,terminal-class :foreground ,terminal-pitchkai-green-l)))

   `(helm-ls-git-deleted-not-staged-face
     ((,class :foreground ,pitchkai-red)
      (,terminal-class :foreground ,terminal-pitchkai-red)))

   `(helm-ls-git-deleted-and-staged-face
     ((,class :foreground ,pitchkai-red-l)
      (,terminal-class :foreground ,terminal-pitchkai-red-l)))

   `(helm-ls-git-conflict-face
     ((,class :foreground ,pitchkai-yellow)
      (,terminal-class :foreground ,terminal-pitchkai-yellow)))

   ;; hi-lock-mode
   `(hi-yellow
     ((,class (:foreground ,pitchkai-yellow-lc
                           :background ,pitchkai-yellow-hc))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow-lc
                                    :background ,terminal-pitchkai-yellow-hc))))

   `(hi-pink
     ((,class (:foreground ,pitchkai-magenta-lc
                           :background ,pitchkai-magenta-hc))
      (,terminal-class (:foreground ,terminal-pitchkai-magenta-lc
                                    :background ,terminal-pitchkai-magenta-hc))))

   `(hi-green
     ((,class (:foreground ,pitchkai-green-lc
                           :background ,pitchkai-green-hc))
      (,terminal-class (:foreground ,terminal-pitchkai-green-lc
                                    :background ,terminal-pitchkai-green-hc))))

   `(hi-blue
     ((,class (:foreground ,pitchkai-blue-lc
                           :background ,pitchkai-blue-hc))
      (,terminal-class (:foreground ,terminal-pitchkai-blue-lc
                                    :background ,terminal-pitchkai-blue-hc))))

   `(hi-black-b
     ((,class (:foreground ,pitchkai-emph
                           :background ,pitchkai-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-emph
                                    :background ,terminal-pitchkai-bg
                                    :weight bold))))

   `(hi-blue-b
     ((,class (:foreground ,pitchkai-blue-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue-lc
                                    :weight bold))))

   `(hi-green-b
     ((,class (:foreground ,pitchkai-green-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-green-lc
                                    :weight bold))))

   `(hi-red-b
     ((,class (:foreground ,pitchkai-red
                           :weight bold))))

   `(hi-black-hb
     ((,class (:foreground ,pitchkai-emph
                           :background ,pitchkai-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-emph
                                    :background ,terminal-pitchkai-bg
                                    :weight bold))))

   ;; highlight-changes
   `(highlight-changes
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(highlight-changes-delete
     ((,class (:foreground ,pitchkai-red
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,class (:background ,pitchkai-gray))
      (,terminal-class (:background ,terminal-pitchkai-gray))))

   `(highlight-indentation-current-column-face
     ((,class (:background ,pitchkai-gray))
      (,terminal-class (:background ,terminal-pitchkai-gray))))

   ;; hl-line-mode
   `(hl-line
     ((,class (:background ,pitchkai-highlight-line))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line))))

   `(hl-line-face
     ((,class (:background ,pitchkai-highlight-line))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line))))

   ;; ido-mode
   `(ido-first-match
     ((,class (:foreground ,pitchkai-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight normal))))

   `(ido-only-match
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-yellow
                                    :weight normal))))

   `(ido-subdir
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(ido-incomplete-regexp
     ((,class (:foreground ,pitchkai-red
                           :weight bold ))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :weight bold ))))

   `(ido-indicator
     ((,class (:background ,pitchkai-red
                           :foreground ,pitchkai-bg
                           :width condensed))
      (,terminal-class (:background ,terminal-pitchkai-red
                                    :foreground ,terminal-pitchkai-bg
                                    :width condensed))))

   `(ido-virtual
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   ;; ivy
   `(ivy-subdir
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(ivy-minibuffer-match-face-1
     ((,class (:foreground ,pitchkai-comments
                           :underline nil))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :underline nil))))

   `(ivy-minibuffer-match-face-2
     ((,class (:foreground ,pitchkai-violet-l
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-violet-l
                                    :underline t))))

   `(ivy-minibuffer-match-face-3
     ((,class (:foreground ,pitchkai-magenta-l
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-magenta-l
                                    :underline t))))

   `(ivy-minibuffer-match-face-4
     ((,class (:foreground ,pitchkai-cyan-l
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan-l
                                    :underline t))))

   `(ivy-match-required-face
     ((,class (:foreground ,pitchkai-red-plain))
      (,terminal-class (:foreground ,pitchkai-red-plain))))

   `(ivy-current-match
     ((,class (:background ,pitchkai-cyan-d
                           :foreground ,pitchkai-cyan-l
                           :weight bold))
      (,terminal-class (:background ,terminal-pitchkai-cyan-d
                                    :foreground ,terminal-pitchkai-cyan-l
                                    :weight bold))))
   `(ivy-highlight-face
     ((,class (:background ,pitchkai-gray))
      (,terminal-class (:background ,pitchkai-gray))))

   ;; jabber

   `(jabber-activity-face
     ((,class (:weight bold
                       :foreground ,pitchkai-red))
      (,terminal-class (:weight bold
                                :foreground ,terminal-pitchkai-red))))

   `(jabber-activity-personal-face
     ((,class (:weight bold
                       :foreground ,pitchkai-blue))
      (,terminal-class (:weight bold
                                :foreground ,terminal-pitchkai-blue))))

   `(jabber-chat-error
     ((,class (:weight bold
                       :foreground ,pitchkai-red))
      (,terminal-class (:weight bold
                                :foreground ,terminal-pitchkai-red))))

   `(jabber-chat-prompt-foreign
     ((,class (:weight bold
                       :foreground ,pitchkai-red))
      (,terminal-class (:weight bold
                                :foreground ,terminal-pitchkai-red))))

   `(jabber-chat-prompt-local
     ((,class (:weight bold
                       :foreground ,pitchkai-blue))
      (,terminal-class (:weight bold
                                :foreground ,terminal-pitchkai-blue))))

   `(jabber-chat-prompt-system
     ((,class (:weight bold
                       :foreground ,pitchkai-green))
      (,terminal-class (:weight bold
                                :foreground ,terminal-pitchkai-green))))

   `(jabber-chat-text-foreign
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(jabber-chat-text-local
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(jabber-chat-rare-time-face
     ((,class (:underline t
                          :foreground ,pitchkai-green))
      (,terminal-class (:underline t
                                   :foreground ,terminal-pitchkai-green))))

   `(jabber-roster-user-away
     ((,class (:slant italic
                      :foreground ,pitchkai-green))
      (,terminal-class (:slant italic
                               :foreground ,terminal-pitchkai-green))))

   `(jabber-roster-user-chatty
     ((,class (:weight bold
                       :foreground ,pitchkai-orange))
      (,terminal-class (:weight bold
                                :foreground ,terminal-pitchkai-orange))))

   `(jabber-roster-user-dnd
     ((,class (:slant italic
                      :foreground ,pitchkai-red))
      (,terminal-class (:slant italic
                               :foreground ,terminal-pitchkai-red))))

   `(jabber-roster-user-error
     ((,class (:weight light
                       :slant italic
                       :foreground ,pitchkai-red))
      (,terminal-class (:weight light
                                :slant italic
                                :foreground ,terminal-pitchkai-red))))

   `(jabber-roster-user-offline
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(jabber-roster-user-online
     ((,class (:weight bold
                       :foreground ,pitchkai-blue))
      (,terminal-class (:weight bold
                                :foreground ,terminal-pitchkai-blue))))

   `(jabber-roster-user-xa
     ((,class (:slant italic
                      :foreground ,pitchkai-magenta))
      (,terminal-class (:slant italic
                               :foreground ,terminal-pitchkai-magenta))))

   ;; js2-mode colors
   `(js2-error
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(js2-external-variable
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(js2-function-param
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(js2-instance-member
     ((,class (:foreground ,pitchkai-magenta))
      (,terminal-class (:foreground ,terminal-pitchkai-magenta))))

   `(js2-jsdoc-html-tag-delimiter
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   `(js2-jsdoc-html-tag-name
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(js2-object-property
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(js2-function-call
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(js2-jsdoc-tag
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   `(js2-jsdoc-type
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(js2-jsdoc-value
     ((,class (:foreground ,pitchkai-violet))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))

   `(js2-magic-paren
     ((,class (:underline t))
      (,terminal-class (:underline t))))

   `(js2-private-function-call
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(js2-private-member
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(js2-warning
     ((,class (:underline ,pitchkai-orange))
      (,terminal-class (:underline ,terminal-pitchkai-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,class (:inherit bold))
      (,terminal-class (:inherit bold))))

   ;; Emacs native line numbers
   `(line-number
     ((,class (:foreground ,pitchkai-highlight
                           :background ,s-fringe-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :background ,terminal-s-fringe-bg))))
   ;; linum-mode
   `(linum
     ((,class (:foreground ,pitchkai-highlight
                           :background ,s-fringe-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :background ,terminal-s-fringe-bg))))

   ;; lusty-explorer
   `(lusty-directory-face
     ((,class (:inherit dipitchkai-red-directory))
      (,terminal-class (:inherit dipitchkai-red-directory))))

   `(lusty-file-face
     ((,class nil)
      (,terminal-class nil)))

   `(lusty-match-face
     ((,class (:inherit ido-first-match))
      (,terminal-class (:inherit ido-first-match))))

   `(lusty-slash-face
     ((,class (:foreground ,pitchkai-cyan
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :weight bold))))

   ;; magit
   ;;
   ;; TODO: Add supports for all magit faces
   ;; https://github.com/magit/magit/search?utf8=%E2%9C%93&q=face
   ;;
   `(magit-diff-added
     ((,class (:foreground ,pitchkai-green-plain
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :background ,terminal-pitchkai-bg))))

   `(magit-diff-added-highlight
     ((,class (:foreground ,pitchkai-green-plain
                           :background ,pitchkai-highlight-line))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :background ,terminal-pitchkai-highlight-line))))

   `(magit-diff-removed
     ((,class (:foreground ,pitchkai-red-plain
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :background ,terminal-pitchkai-bg))))

   `(magit-diff-removed-highlight
     ((,class (:foreground ,pitchkai-red-plain
                           :background ,pitchkai-highlight-line))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :background ,terminal-pitchkai-highlight-line))))

   `(magit-section-highlight
     ((,class (:background ,pitchkai-highlight-line))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line))))

   `(magit-section-title
     ((,class (:foreground ,pitchkai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight bold))))

   `(magit-branch
     ((,class (:foreground ,pitchkai-orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-orange
                                    :weight bold))))

   `(magit-cherry-equivalent
     ((,class (:foreground ,pitchkai-magenta))
      (,terminal-class (:foreground ,terminal-pitchkai-magenta))))

   `(magit-cherry-unmatched
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   `(magit-head
     ((,class (:foreground ,pitchkai-violet))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))

   `(magit-branch-local
     ((,class (:foreground ,pitchkai-violet))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))

   `(magit-branch-remote
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(magit-section-heading
     ((,class (:foreground ,pitchkai-yellow :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow :weight bold))))

   `(magit-process-ok
     ((,class (:foreground ,pitchkai-green-plain
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :weight bold))))

   `(magit-process-ng
     ((,class (:foreground ,pitchkai-red-plain
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :weight bold))))

   `(magit-item-highlight
     ((,class (:background ,pitchkai-highlight-line
                           :weight unspecified))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :weight unspecified))))

   `(magit-log-author
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   `(magit-log-graph
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(magit-log-head-label-bisect-bad
     ((,class (:background ,pitchkai-red-hc
                           :foreground ,pitchkai-red-lc
                           :box 1))
      (,terminal-class (:background ,terminal-pitchkai-red-hc
                                    :foreground ,terminal-pitchkai-red-lc
                                    :box 1))))

   `(magit-log-head-label-bisect-good
     ((,class (:background ,pitchkai-green-hc
                           :foreground ,pitchkai-green-lc
                           :box 1))
      (,terminal-class (:background ,terminal-pitchkai-green-hc
                                    :foreground ,terminal-pitchkai-green-lc
                                    :box 1))))

   `(magit-log-head-label-default
     ((,class (:background ,pitchkai-highlight-line
                           :box 1))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :box 1))))

   `(magit-log-head-label-local
     ((,class (:background ,pitchkai-blue-lc
                           :foreground ,pitchkai-blue-hc
                           :box 1))
      (,terminal-class (:background ,terminal-pitchkai-blue-lc
                                    :foreground ,terminal-pitchkai-blue-hc
                                    :box 1))))

   `(magit-log-head-label-patches
     ((,class (:background ,pitchkai-red-lc
                           :foreground ,pitchkai-red-hc
                           :box 1))
      (,terminal-class (:background ,terminal-pitchkai-red-lc
                                    :foreground ,terminal-pitchkai-red-hc
                                    :box 1))))

   `(magit-log-head-label-remote
     ((,class (:background ,pitchkai-green-lc
                           :foreground ,pitchkai-green-hc
                           :box 1))
      (,terminal-class (:background ,terminal-pitchkai-green-lc
                                    :foreground ,terminal-pitchkai-green-hc
                                    :box 1))))

   `(magit-log-head-label-tags
     ((,class (:background ,pitchkai-yellow-lc
                           :foreground ,pitchkai-yellow-hc
                           :box 1))
      (,terminal-class (:background ,terminal-pitchkai-yellow-lc
                                    :foreground ,terminal-pitchkai-yellow-hc
                                    :box 1))))

   `(magit-log-sha1
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(magit-reflog-amend
     ((,class (:foreground ,pitchkai-violet))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))
   `(magit-reflog-rebase
     ((,class (:foreground ,pitchkai-violet))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))
   `(magit-reflog-checkout
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))
   `(magit-reflog-reset
     ((,class (:foreground ,pitchkai-red-plain))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))
   `(magit-reflog-commit
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))
   `(magit-reflog-merge
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))
   `(magit-reflog-cherry-pick
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))
   `(magit-reflog-other
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))
   `(magit-reflog-remote
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   ;; man
   `(Man-overstrike
     ((,class (:foreground ,pitchkai-blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :weight bold))))

   `(Man-reverse
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(Man-underline
     ((,class (:foreground ,pitchkai-green :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-green :underline t))))

   ;; monky
   `(monky-section-title
     ((,class (:foreground ,pitchkai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight bold))))

   `(monky-diff-title
     ((,class (:foreground ,pitchkai-cyan-l
                           :background ,pitchkai-gray-ld))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan-l
                                    :background ,terminal-pitchkai-gray-d))))

   `(monky-diff-add
     ((,class (:foreground ,pitchkai-green-plain))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(monky-diff-del
     ((,class (:foreground ,pitchkai-red-plain))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(markdown-header-face-1
     ((,class (:inherit markdown-header-face
                        :height ,pitchkai-height-plus-4))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,pitchkai-height-plus-4))))

   `(markdown-header-face-2
     ((,class (:inherit markdown-header-face
                        :height ,pitchkai-height-plus-3))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,pitchkai-height-plus-3))))

   `(markdown-header-face-3
     ((,class (:inherit markdown-header-face
                        :height ,pitchkai-height-plus-2))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,pitchkai-height-plus-2))))

   `(markdown-header-face-4
     ((,class (:inherit markdown-header-face
                        :height ,pitchkai-height-plus-1))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,pitchkai-height-plus-1))))

   `(markdown-header-face-5
     ((,class (:inherit markdown-header-face))
      (,terminal-class (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,class (:inherit markdown-header-face))
      (,terminal-class (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(message-header-name
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(message-header-other
     ((,class (:foreground ,pitchkai-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :weight normal))))

   `(message-header-to
     ((,class (:foreground ,pitchkai-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :weight normal))))

   `(message-header-cc
     ((,class (:foreground ,pitchkai-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :weight normal))))

   `(message-header-newsgroups
     ((,class (:foreground ,pitchkai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight bold))))

   `(message-header-subject
     ((,class (:foreground ,pitchkai-cyan
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :weight normal))))

   `(message-header-xheader
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   `(message-mml
     ((,class (:foreground ,pitchkai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight bold))))

   `(message-separator
     ((,class (:foreground ,pitchkai-comments
                           :slant italic))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :slant italic))))

   ;; mew
   `(mew-face-header-subject
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(mew-face-header-from
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(mew-face-header-date
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(mew-face-header-to
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(mew-face-header-key
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(mew-face-header-private
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(mew-face-header-important
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(mew-face-header-marginal
     ((,class (:foreground ,pitchkai-fg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :weight bold))))

   `(mew-face-header-warning
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(mew-face-header-xmew
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(mew-face-header-xmew-bad
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(mew-face-body-url
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(mew-face-body-comment
     ((,class (:foreground ,pitchkai-fg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :slant italic))))

   `(mew-face-body-cite1
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(mew-face-body-cite2
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(mew-face-body-cite3
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(mew-face-body-cite4
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(mew-face-body-cite5
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(mew-face-mark-review
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(mew-face-mark-escape
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(mew-face-mark-delete
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(mew-face-mark-unlink
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(mew-face-mark-refile
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(mew-face-mark-unread
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(mew-face-eof-message
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(mew-face-eof-part
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   ;; mingus
   `(mingus-directory-face
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(mingus-pausing-face
     ((,class (:foreground ,pitchkai-magenta))
      (,terminal-class (:foreground ,terminal-pitchkai-magenta))))

   `(mingus-playing-face
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   `(mingus-playlist-face
     ((,class (:foreground ,pitchkai-cyan ))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan ))))

   `(mingus-song-file-face
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(mingus-stopped-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   ;; mmm
   `(mmm-init-submode-face
     ((,class (:background ,pitchkai-violet-d))
      (,terminal-class (:background ,terminal-pitchkai-violet-d))))

   `(mmm-cleanup-submode-face
     ((,class (:background ,pitchkai-orange-d))
      (,terminal-class (:background ,terminal-pitchkai-orange-d))))

   `(mmm-declaration-submode-face
     ((,class (:background ,pitchkai-cyan-d))
      (,terminal-class (:background ,terminal-pitchkai-cyan-d))))

   `(mmm-comment-submode-face
     ((,class (:background ,pitchkai-blue-d))
      (,terminal-class (:background ,terminal-pitchkai-blue-d))))

   `(mmm-output-submode-face
     ((,class (:background ,pitchkai-red-d))
      (,terminal-class (:background ,terminal-pitchkai-red-d))))

   `(mmm-special-submode-face
     ((,class (:background ,pitchkai-green-d))
      (,terminal-class (:background ,terminal-pitchkai-green-d))))

   `(mmm-code-submode-face
     ((,class (:background ,pitchkai-gray))
      (,terminal-class (:background ,terminal-pitchkai-gray))))

   `(mmm-default-submode-face
     ((,class (:background ,pitchkai-gray-d))
      (,terminal-class (:background ,terminal-pitchkai-gray-d))))

   ;; moccur
   `(moccur-current-line-face
     ((,class (:underline t))
      (,terminal-class (:underline t))))

   `(moccur-edit-done-face
     ((,class (:foreground ,pitchkai-comments
                           :background ,pitchkai-bg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :background ,terminal-pitchkai-bg
                                    :slant italic))))

   `(moccur-edit-face
     ((,class (:background ,pitchkai-yellow
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-yellow
                                    :foreground ,terminal-pitchkai-bg))))

   `(moccur-edit-file-face
     ((,class (:background ,pitchkai-highlight-line))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line))))

   `(moccur-edit-reject-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(moccur-face
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-emph
                           :weight bold))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-emph
                                    :weight bold))))

   `(search-buffers-face
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-emph
                           :weight bold))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-emph
                                    :weight bold))))

   `(search-buffers-header-face
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-yellow
                           :weight bold))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-yellow
                                    :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face
     ((,class (:foreground ,pitchkai-green
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-2-face
     ((,class (:foreground ,pitchkai-blue
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-3-face
     ((,class (:foreground ,pitchkai-orange
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-orange
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-4-face
     ((,class (:foreground ,pitchkai-yellow
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-5-face
     ((,class (:foreground ,pitchkai-cyan
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-6-face
     ((,class (:foreground ,pitchkai-green
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-7-face
     ((,class (:foreground ,pitchkai-blue
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :slant italic
                                    :weight normal))))

   `(mu4e-flagged-face
     ((,class (:foreground ,pitchkai-magenta
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-magenta
                                    :weight bold))))

   `(mu4e-view-url-number-face
     ((,class (:foreground ,pitchkai-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight normal))))

   `(mu4e-warning-face
     ((,class (:foreground ,pitchkai-red
                           :slant normal
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :slant normal
                                    :weight bold))))

   `(mu4e-header-highlight-face
     ((,class (:inherit unspecified
                        :foreground unspecified
                        :background ,pitchkai-highlight-line
                        :underline ,pitchkai-emph
                        :weight normal))
      (,terminal-class (:inherit unspecified
                                 :foreground unspecified
                                 :background ,terminal-pitchkai-highlight-line
                                 :underline ,terminal-pitchkai-emph
                                 :weight normal))))


   `(mu4e-draft-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(mu4e-footer-face
     ((,class (:inherit font-lock-comment-face))
      (,terminal-class (:inherit font-lock-comment-face))))

   `(mu4e-forwarded-face
     ((,class (:inherit font-lock-builtin-face
                        :weight normal))
      (,terminal-class (:inherit font-lock-builtin-face
                                 :weight normal))))

   `(mu4e-header-face
     ((,class (:inherit default))
      (,terminal-class (:inherit default))))

   `(mu4e-header-marks-face
     ((,class (:inherit font-lock-preprocessor-face))
      (,terminal-class (:inherit font-lock-preprocessor-face))))

   `(mu4e-header-title-face
     ((,class (:inherit font-lock-type-face))
      (,terminal-class (:inherit font-lock-type-face))))

   `(mu4e-highlight-face
     ((,class (:inherit font-lock-pseudo-keyword-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-pseudo-keyword-face
                                 :weight bold))))

   `(mu4e-moved-face
     ((,class (:inherit font-lock-comment-face
                        :slant italic))
      (,terminal-class (:inherit font-lock-comment-face
                                 :slant italic))))

   `(mu4e-ok-face
     ((,class (:inherit font-lock-comment-face
                        :slant normal
                        :weight bold))
      (,terminal-class (:inherit font-lock-comment-face
                                 :slant normal
                                 :weight bold))))

   `(mu4e-replied-face
     ((,class (:inherit font-lock-function-name-face
                        :weight normal))
      (,terminal-class (:inherit font-lock-function-face
                                 :weight normal))))

   `(mu4e-system-face
     ((,class (:inherit font-lock-comment-face
                        :slant italic))
      (,terminal-class (:inherit font-lock-comment-face
                                 :slant italic))))

   `(mu4e-title-face
     ((,class (:inherit font-lock-type-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-type-face
                                 :weight bold))))

   `(mu4e-trashed-face
     ((,class (:inherit font-lock-comment-face
                        :strike-through t))
      (,terminal-class (:inherit font-lock-comment-face
                                 :strike-through t))))

   `(mu4e-unread-face
     ((,class (:inherit font-lock-keyword-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-keyword-face
                                 :weight bold))))

   `(mu4e-view-attach-number-face
     ((,class (:inherit font-lock-variable-name-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-variable-name-face
                                 :weight bold))))

   `(mu4e-view-contact-face
     ((,class (:foreground ,pitchkai-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :weight normal))))

   `(mu4e-view-header-key-face
     ((,class (:inherit message-header-name
                        :weight normal))
      (,terminal-class (:inherit message-header-name
                                 :weight normal))))

   `(mu4e-view-header-value-face
     ((,class (:foreground ,pitchkai-cyan
                           :weight normal
                           :slant normal))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :weight normal
                                    :slant normal))))

   `(mu4e-view-link-face
     ((,class (:inherit link))
      (,terminal-class (:inherit link))))

   `(mu4e-view-special-header-value-face
     ((,class (:foreground ,pitchkai-blue
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :weight normal
                                    :underline nil))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,class (:background ,pitchkai-highlight-line))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line))))

   ;; nav
   `(nav-face-heading
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(nav-face-button-num
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   `(nav-face-dir
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(nav-face-hdir
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(nav-face-file
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(nav-face-hfile
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,class (:background ,pitchkai-highlight-line))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line))))

   ;; neo-tree
   `(neo-banner-face
     ((,class (:foreground ,pitchkai-blue
                           :background ,pitchkai-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :background ,terminal-pitchkai-bg
                                    :weight bold))))


   `(neo-header-face
     ((,class (:foreground ,pitchkai-emph
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-emph
                                    :background ,terminal-pitchkai-bg))))

   `(neo-root-dir-face
     ((,class (:foreground ,pitchkai-green
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :background ,terminal-pitchkai-bg))))

   `(neo-dir-link-face
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :background ,terminal-pitchkai-bg))))

   `(neo-file-link-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(neo-button-face
     ((,class (:underline nil))
      (,terminal-class (:underline nil))))

   `(neo-expand-btn-face
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(neo-vc-default-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(neo-vc-user-face
     ((,class (:foreground ,pitchkai-red
                           :slant italic))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :slant italic))))

   `(neo-vc-up-to-date-face
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(neo-vc-edited-face
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(neo-vc-needs-update-face
     ((,class (:underline t))
      (,terminal-class (:underline t))))

   `(neo-vc-needs-merge-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(neo-vc-unlocked-changes-face
     ((,class (:foreground ,pitchkai-red
                           :background ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :background ,terminal-pitchkai-comments))))

   `(neo-vc-added-face
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(neo-vc-removed-face
     ((,class (:strike-through t))
      (,terminal-class (:strike-through t))))

   `(neo-vc-conflict-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(neo-vc-missing-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(neo-vc-ignored-face
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))


   ;; org-mode
   `(org-agenda-structure
     ((,class (:foreground ,pitchkai-emph
                           :background ,pitchkai-highlight-line
                           :weight bold
                           :slant normal
                           :inverse-video nil
                           :height ,pitchkai-height-plus-1
                           :underline nil
                           :box (:line-width 2 :color ,pitchkai-bg)))
      (,terminal-class (:foreground ,terminal-pitchkai-emph
                                    :background ,terminal-pitchkai-highlight-line
                                    :weight bold
                                    :slant normal
                                    :inverse-video nil
                                    :height ,pitchkai-height-plus-1
                                    :underline nil
                                    :box (:line-width 2 :color ,terminal-pitchkai-bg)))))

   `(org-agenda-calendar-event
     ((,class (:foreground ,pitchkai-emph))
      (,terminal-class (:foreground ,terminal-pitchkai-emph))))

   `(org-agenda-calendar-sexp
     ((,class (:foreground ,pitchkai-fg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :slant italic))))

   `(org-agenda-date
     ((,class (:foreground ,pitchkai-comments
                           :background ,pitchkai-bg
                           :weight normal
                           :inverse-video nil
                           :overline nil
                           :slant normal
                           :height 1.0
                           :box (:line-width 2 :color ,pitchkai-bg)))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :background ,terminal-pitchkai-bg
                                    :weight normal
                                    :inverse-video nil
                                    :overline nil
                                    :slant normal
                                    :height 1.0
                                    :box (:line-width 2 :color ,terminal-pitchkai-bg)))) t)

   `(org-agenda-date-weekend
     ((,class (:inherit org-agenda-date
                        :inverse-video nil
                        :background unspecified
                        :foreground ,pitchkai-comments
                        :weight unspecified
                        :underline t
                        :overline nil
                        :box unspecified))
      (,terminal-class (:inherit org-agenda-date
                                 :inverse-video nil
                                 :background unspecified
                                 :foreground ,terminal-pitchkai-comments
                                 :weight unspecified
                                 :underline t
                                 :overline nil
                                 :box unspecified))) t)

   `(org-agenda-date-today
     ((,class (:inherit org-agenda-date
                        :inverse-video t
                        :weight bold
                        :underline unspecified
                        :overline nil
                        :box unspecified
                        :foreground ,pitchkai-blue
                        :background ,pitchkai-bg))
      (,terminal-class (:inherit org-agenda-date
                                 :inverse-video t
                                 :weight bold
                                 :underline unspecified
                                 :overline nil
                                 :box unspecified
                                 :foreground ,terminal-pitchkai-blue
                                 :background ,terminal-pitchkai-bg))) t)

   `(org-agenda-done
     ((,class (:foreground ,pitchkai-comments
                           :slant italic))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :slant italic))) t)

   `(org-archived
     ((,class (:foreground ,pitchkai-comments
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :weight normal))))

   `(org-block
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(org-block-begin-line
     ((,class (:foreground ,pitchkai-comments
                           :slant italic))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :slant italic))))

   `(org-checkbox
     ((,class (:background ,pitchkai-gray
                           :foreground ,pitchkai-violet
                           :box nil))
      (,terminal-class (:background ,terminal-pitchkai-gray
                                    :foreground ,terminal-pitchkai-violet
                                    :box nil))))

   `(org-code
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(org-date
     ((,class (:foreground ,pitchkai-blue
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :underline t))))

   `(org-done
     ((,class (:weight bold
                       :foreground ,pitchkai-green))
      (,terminal-class (:weight bold
                                :foreground ,terminal-pitchkai-green))))

   `(org-ellipsis
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(org-formula
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(org-headline-done
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(org-hide
     ((,class (:foreground ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-bg))))

   `(org-level-1
     ((,class (:inherit ,s-variable-pitch
                        :height ,pitchkai-height-plus-4
                        :foreground ,pitchkai-orange))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :height ,pitchkai-height-plus-4
                                 :foreground ,terminal-pitchkai-orange))))

   `(org-level-2
     ((,class (:inherit ,s-variable-pitch
                        :height ,pitchkai-height-plus-3
                        :foreground ,pitchkai-green))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :height ,pitchkai-height-plus-3
                                 :foreground ,terminal-pitchkai-green))))

   `(org-level-3
     ((,class (:inherit ,s-variable-pitch
                        :height ,pitchkai-height-plus-2
                        :foreground ,pitchkai-blue))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :height ,pitchkai-height-plus-2
                                 :foreground ,terminal-pitchkai-blue))))

   `(org-level-4
     ((,class (:inherit ,s-variable-pitch
                        :height ,pitchkai-height-plus-1
                        :foreground ,pitchkai-yellow))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :height ,pitchkai-height-plus-1
                                 :foreground ,terminal-pitchkai-yellow))))

   `(org-level-5
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,pitchkai-cyan))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-pitchkai-cyan))))

   `(org-level-6
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,pitchkai-green))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-pitchkai-green))))

   `(org-level-7
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,pitchkai-red))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-pitchkai-red))))

   `(org-level-8
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,pitchkai-blue))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-pitchkai-blue))))

   `(org-link
     ((,class (:foreground ,pitchkai-yellow
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :underline t))))

   `(org-sexp-date
     ((,class (:foreground ,pitchkai-violet))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))

   `(org-scheduled
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(org-scheduled-previously
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   `(org-scheduled-today
     ((,class (:foreground ,pitchkai-blue
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :weight normal))))

   `(org-special-keyword
     ((,class (:foreground ,pitchkai-comments
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :weight bold))))

   `(org-table
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(org-tag
     ((,class (:weight bold))
      (,terminal-class (:weight bold))))

   `(org-time-grid
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(org-todo
     ((,class (:foreground ,pitchkai-red
                           :weight bold)))
     ((,terminal-class (:foreground ,terminal-pitchkai-red
                                    :weight bold))))

   `(org-upcoming-deadline
     ((,class (:foreground ,pitchkai-yellow
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight normal
                                    :underline nil))))

   `(org-warning
     ((,class (:foreground ,pitchkai-orange
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-pitchkai-orange
                                    :weight normal
                                    :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,class (:background ,pitchkai-blue-lc
                           :foreground ,pitchkai-blue-hc))
      (,terminal-class (:background ,terminal-pitchkai-blue-lc
                                    :foreground ,terminal-pitchkai-blue-hc))))

   `(org-habit-clear-future-face
     ((,class (:background ,pitchkai-blue-l))
      (,terminal-class (:background ,terminal-pitchkai-blue-l))))

   `(org-habit-ready-face
     ((,class (:background ,pitchkai-green-plain))
      (,terminal-class (:background ,terminal-pitchkai-green))))

   `(org-habit-ready-future-face
     ((,class (:background ,pitchkai-green-lc))
      (,terminal-class (:background ,terminal-pitchkai-green-lc))))

   `(org-habit-alert-face
     ((,class (:background ,pitchkai-yellow))
      (,terminal-class (:background ,terminal-pitchkai-yellow))))

   `(org-habit-alert-future-face
     ((,class (:background ,pitchkai-yellow-lc))
      (,terminal-class (:background ,terminal-pitchkai-yellow-lc))))

   `(org-habit-overdue-face
     ((,class (:background ,pitchkai-red-plain))
      (,terminal-class (:background ,terminal-pitchkai-red))))

   `(org-habit-overdue-future-face
     ((,class (:background ,pitchkai-red-hc))
      (,terminal-class (:background ,terminal-pitchkai-red-hc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(org-agenda-restriction-lock
     ((,class (:background ,pitchkai-yellow))
      (,terminal-class (:background ,terminal-pitchkai-yellow))))

   `(org-clock-overlay
     ((,class (:background ,pitchkai-yellow))
      (,terminal-class (:background ,terminal-pitchkai-yellow))))

   `(org-column
     ((,class (:background ,pitchkai-highlight-line
                           :strike-through nil
                           :underline nil
                           :slant normal
                           :weight normal
                           :inherit default))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :strike-through nil
                                    :underline nil
                                    :slant normal
                                    :weight normal
                                    :inherit default))))

   `(org-column-title
     ((,class (:background ,pitchkai-highlight-line
                           :underline t
                           :weight bold))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :underline t
                                    :weight bold))))

   `(org-date-selected
     ((,class (:foreground ,pitchkai-red
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :inverse-video t))))

   `(org-document-info
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(org-document-title
     ((,class (:foreground ,pitchkai-emph
                           :weight bold
                           :height ,pitchkai-height-plus-4))
      (,terminal-class (:foreground ,terminal-pitchkai-emph
                                    :weight bold
                                    :height ,pitchkai-height-plus-4))))

   `(org-drawer
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   `(org-footnote
     ((,class (:foreground ,pitchkai-magenta
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-magenta
                                    :underline t))))

   `(org-latex-and-export-specials
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(org-mode-line-clock-overrun
     ((,class (:inherit mode-line))
      (,terminal-class (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,class (:inherit org-level-1))
      (,terminal-class (:inherit org-level-1))))

   `(outline-2
     ((,class (:inherit org-level-2))
      (,terminal-class (:inherit org-level-2))))

   `(outline-3
     ((,class (:inherit org-level-3))
      (,terminal-class (:inherit org-level-3))))

   `(outline-4
     ((,class (:inherit org-level-4))
      (,terminal-class (:inherit org-level-4))))

   `(outline-5
     ((,class (:inherit org-level-5))
      (,terminal-class (:inherit org-level-5))))

   `(outline-6
     ((,class (:inherit org-level-6))
      (,terminal-class (:inherit org-level-6))))

   `(outline-7
     ((,class (:inherit org-level-7))
      (,terminal-class (:inherit org-level-7))))

   `(outline-8
     ((,class (:inherit org-level-8))
      (,terminal-class (:inherit org-level-8))))

   ;; parenface
   `(paren-face
     ((,terminal-class (:foreground ,pitchkai-comments))))

   ;; perspective
   `(persp-selected-face
     ((,class (:foreground ,pitchkai-violet
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-violet
                                    :weight bold))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,class (:foreground ,pitchkai-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight normal))))

   ;; popup
   `(popup-face
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-fg))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-fg))))

   `(popup-isearch-match
     ((,class (:background ,pitchkai-green))
      (,terminal-class (:background ,terminal-pitchkai-green))))

   `(popup-menu-face
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-fg))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-fg))))

   `(popup-menu-mouse-face
     ((,class (:background ,pitchkai-blue
                           :foreground ,pitchkai-fg))
      (,terminal-class (:background ,terminal-pitchkai-blue
                                    :foreground ,terminal-pitchkai-fg))))

   `(popup-menu-selection-face
     ((,class (:background ,pitchkai-magenta
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-magenta
                                    :foreground ,terminal-pitchkai-bg))))

   `(popup-scroll-bar-background-face
     ((,class (:background ,pitchkai-comments))
      (,terminal-class (:background ,terminal-pitchkai-comments))))

   `(popup-scroll-bar-foreground-face
     ((,class (:background ,pitchkai-emph))
      (,terminal-class (:background ,terminal-pitchkai-emph))))

   `(popup-tip-face
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-fg))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-fg))))

   ;; powerline
   `(powerline-active1
     ((,class (:background ,s-powerline-active1-bg :foreground ,pitchkai-fg))
      (,terminal-class (:background ,terminal-pitchkai-gray :foreground ,pitchkai-fg))))

   `(powerline-active2
     ((,class (:background ,s-powerline-active2-bg :foreground ,pitchkai-fg))
      (,terminal-class (:background ,terminal-pitchkai-gray-l :foreground ,pitchkai-fg))))

   `(powerline-inactive1
     ((,class (:background ,s-powerline-inactive1-bg))
      (,terminal-class (:background ,terminal-pitchkai-gray-d))))

   `(powerline-inactive2
     ((,class (:background ,s-powerline-inactive2-bg))
      (,terminal-class (:background ,terminal-pitchkai-gray))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,class (:foreground ,pitchkai-violet))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))

   `(rainbow-delimiters-depth-2-face
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(rainbow-delimiters-depth-3-face
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(rainbow-delimiters-depth-4-face
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(rainbow-delimiters-depth-5-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(rainbow-delimiters-depth-6-face
     ((,class (:foreground ,pitchkai-magenta-d))
      (,terminal-class (:foreground ,terminal-pitchkai-magenta))))

   `(rainbow-delimiters-depth-7-face
     ((,class (:foreground ,pitchkai-violet))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))

   `(rainbow-delimiters-depth-8-face
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(rainbow-delimiters-depth-9-face
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(rainbow-delimiters-unmatched-face
     ((,class (:foreground ,pitchkai-fg
                           :background ,pitchkai-bg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :background ,terminal-pitchkai-bg
                                    :inverse-video t))))

   ;; rhtm-mode
   `(erb-face
     ((,class (:foreground ,pitchkai-emph
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-emph
                                    :background ,terminal-pitchkai-bg))))

   `(erb-delim-face
     ((,class (:foreground ,pitchkai-cyan
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :background ,terminal-pitchkai-bg))))

   `(erb-exec-face
     ((,class (:foreground ,pitchkai-emph
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-emph
                                    :background ,terminal-pitchkai-bg))))

   `(erb-exec-delim-face
     ((,class (:foreground ,pitchkai-cyan
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :background ,terminal-pitchkai-bg))))

   `(erb-out-face
     ((,class (:foreground ,pitchkai-emph
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-emph
                                    :background ,terminal-pitchkai-bg))))

   `(erb-out-delim-face
     ((,class (:foreground ,pitchkai-cyan
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :background ,terminal-pitchkai-bg))))

   `(erb-comment-face
     ((,class (:foreground ,pitchkai-emph
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-emph
                                    :background ,terminal-pitchkai-bg))))

   `(erb-comment-delim-face
     ((,class (:foreground ,pitchkai-cyan
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :background ,terminal-pitchkai-bg))))

   ;; rst-mode
   `(rst-level-1-face
     ((,class (:background ,pitchkai-yellow
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-yellow
                                    :foreground ,terminal-pitchkai-bg))))

   `(rst-level-2-face
     ((,class (:background ,pitchkai-cyan
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-cyan
                                    :foreground ,terminal-pitchkai-bg))))

   `(rst-level-3-face
     ((,class (:background ,pitchkai-blue
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-blue
                                    :foreground ,terminal-pitchkai-bg))))

   `(rst-level-4-face
     ((,class (:background ,pitchkai-violet
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-violet
                                    :foreground ,terminal-pitchkai-bg))))

   `(rst-level-5-face
     ((,class (:background ,pitchkai-magenta
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-magenta
                                    :foreground ,terminal-pitchkai-bg))))

   `(rst-level-6-face
     ((,class (:background ,pitchkai-red
                           :foreground ,pitchkai-bg))
      (,terminal-class (:background ,terminal-pitchkai-red
                                    :foreground ,terminal-pitchkai-bg))))

   ;; rpm-mode
   `(rpm-spec-dir-face
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(rpm-spec-doc-face
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(rpm-spec-ghost-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(rpm-spec-macro-face
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(rpm-spec-obsolete-tag-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(rpm-spec-package-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(rpm-spec-section-face
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(rpm-spec-tag-face
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(rpm-spec-var-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,class (:foreground ,pitchkai-violet
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-violet
                                    :weight bold))))

   `(sh-escaped-newline
     ((,class (:foreground ,pitchkai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight bold))))

   `(sh-heredoc
     ((,class (:foreground ,pitchkai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight bold))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,class (:background ,pitchkai-highlight-line))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line))))

   `(sp-wrap-overlay-face
     ((,class (:background ,pitchkai-highlight-line))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line))))

   `(sp-wrap-tag-overlay-face
     ((,class (:background ,pitchkai-highlight-line))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line))))

   `(sp-show-pair-enclosing
     ((,class (:inherit highlight))
      (,terminal-class (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,class (:background ,pitchkai-cyan-l
                           :foreground ,pitchkai-cyan-d
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :background ,terminal-pitchkai-bg
                                    :weight normal
                                    :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,class (:foreground ,pitchkai-red
                           :background ,pitchkai-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :background ,terminal-pitchkai-bg
                                    :weight normal
                                    :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,class (:foreground ,pitchkai-cyan-d
                           :background ,pitchkai-cyan-l
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan-d
                                    :background ,terminal-pitchkai-cyan-l
                                    :weight normal
                                    :inverse-video t))))

   `(show-paren-mismatch
     ((,class (:foreground ,pitchkai-red
                           :background ,pitchkai-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :background ,terminal-pitchkai-bg
                                    :weight normal
                                    :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,class (:foreground ,pitchkai-green
                           :background ,pitchkai-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :background ,terminal-pitchkai-bg
                                    :weight normal
                                    :inverse-video t))))

   `(paren-face-mismatch
     ((,class (:foreground ,pitchkai-red
                           :background ,pitchkai-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :background ,terminal-pitchkai-bg
                                    :weight normal
                                    :inverse-video t))))

   `(paren-face-no-match
     ((,class (:foreground ,pitchkai-red
                           :background ,pitchkai-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :background ,terminal-pitchkai-bg
                                    :weight normal
                                    :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   ;; speedbar
   `(speedbar-button-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,pitchkai-comments))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-pitchkai-comments))))

   `(speedbar-directory-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,pitchkai-blue))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-pitchkai-blue))))

   `(speedbar-file-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,pitchkai-fg))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-pitchkai-fg))))

   `(speedbar-highlight-face
     ((,class (:inherit ,s-variable-pitch
                        :background ,pitchkai-highlight-line))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :background ,terminal-pitchkai-highlight-line))))

   `(speedbar-selected-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,pitchkai-yellow
                        :underline t))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-pitchkai-yellow
                                 :underline t))))

   `(speedbar-separator-face
     ((,class (:inherit ,s-variable-pitch
                        :background ,pitchkai-blue
                        :foreground ,pitchkai-bg
                        :overline ,pitchkai-cyan-lc))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :background ,terminal-pitchkai-blue
                                 :foreground ,terminal-pitchkai-bg
                                 :overline ,terminal-pitchkai-cyan-lc))))

   `(speedbar-tag-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,pitchkai-green))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-pitchkai-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,class (:background ,pitchkai-blue
                           :foreground ,pitchkai-bg
                           :height ,pitchkai-height-plus-1
                           :weight bold))
      (,terminal-class (:background ,terminal-pitchkai-blue
                                    :foreground ,terminal-pitchkai-bg
                                    :height ,pitchkai-height-plus-1
                                    :weight bold))))

   `(sr-editing-path-face
     ((,class (:background ,pitchkai-yellow
                           :foreground ,pitchkai-bg
                           :weight bold
                           :height ,pitchkai-height-plus-1))
      (,terminal-class (:background ,terminal-pitchkai-yellow
                                    :foreground ,terminal-pitchkai-bg
                                    :weight bold
                                    :height ,pitchkai-height-plus-1))))

   `(sr-highlight-path-face
     ((,class (:background ,pitchkai-green
                           :foreground ,pitchkai-bg
                           :weight bold
                           :height ,pitchkai-height-plus-1))
      (,terminal-class (:background ,terminal-pitchkai-green
                                    :foreground ,terminal-pitchkai-bg
                                    :weight bold
                                    :height ,pitchkai-height-plus-1))))

   `(sr-passive-path-face
     ((,class (:background ,pitchkai-comments
                           :foreground ,pitchkai-bg
                           :weight bold
                           :height ,pitchkai-height-plus-1))
      (,terminal-class (:background ,terminal-pitchkai-comments
                                    :foreground ,terminal-pitchkai-bg
                                    :weight bold
                                    :height ,pitchkai-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,class (:inherit dipitchkai-red-marked))
      (,terminal-class (:inherit dipitchkai-red-marked))))

   `(sr-marked-file-face
     ((,class (:inherit dipitchkai-red-marked))
      (,terminal-class (:inherit dipitchkai-red-marked))))

   `(sr-alt-marked-dir-face
     ((,class (:background ,pitchkai-magenta
                           :foreground ,pitchkai-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-pitchkai-magenta
                                    :foreground ,terminal-pitchkai-bg
                                    :weight bold))))

   `(sr-alt-marked-file-face
     ((,class (:background ,pitchkai-magenta
                           :foreground ,pitchkai-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-pitchkai-magenta
                                    :foreground ,terminal-pitchkai-bg
                                    :weight bold))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,class (:inherit dipitchkai-red-directory
                        :weight normal))
      (,terminal-class (:inherit dipitchkai-red-directory
                                 :weight normal))))

   `(sr-symlink-directory-face
     ((,class (:inherit dipitchkai-red-directory
                        :slant italic
                        :weight normal))
      (,terminal-class (:inherit dipitchkai-red-directory
                                 :slant italic
                                 :weight normal))))

   `(sr-symlink-face
     ((,class (:inherit dipitchkai-red-symlink
                        :slant italic
                        :weight normal))
      (,terminal-class (:inherit dipitchkai-red-symlink
                                 :slant italic
                                 :weight normal))))

   `(sr-broken-link-face
     ((,class (:inherit dipitchkai-red-warning
                        :slant italic
                        :weight normal))
      (,terminal-class (:inherit dipitchkai-red-warning
                                 :slant italic
                                 :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(sr-encrypted-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(sr-log-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(sr-packaged-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(sr-html-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(sr-xml-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,class (:background ,pitchkai-red
                           :foreground ,pitchkai-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-pitchkai-red
                                    :foreground ,terminal-pitchkai-bg
                                    :weight bold))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,class (:background unspecified
                           :foreground ,pitchkai-yellow))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-pitchkai-yellow))))

   `(syslog-hour-face
     ((,class (:background unspecified
                           :foreground ,pitchkai-green))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-pitchkai-green))))

   `(syslog-error-face
     ((,class (:background unspecified
                           :foreground ,pitchkai-red
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-pitchkai-red
                                    :weight bold))))

   `(syslog-warn-face
     ((,class (:background unspecified
                           :foreground ,pitchkai-orange
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-pitchkai-orange
                                    :weight bold))))

   `(syslog-info-face
     ((,class (:background unspecified
                           :foreground ,pitchkai-blue
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-pitchkai-blue
                                    :weight bold))))

   `(syslog-debug-face
     ((,class (:background unspecified
                           :foreground ,pitchkai-cyan
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-pitchkai-cyan
                                    :weight bold))))

   `(syslog-su-face
     ((,class (:background unspecified
                           :foreground ,pitchkai-magenta))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-pitchkai-magenta))))

   ;; table
   `(table-cell
     ((,class (:foreground ,pitchkai-fg
                           :background ,pitchkai-highlight-line))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :background ,terminal-pitchkai-highlight-line))))

   ;; term
   `(term-color-black
     ((,class (:foreground ,pitchkai-bg
                           :background ,pitchkai-highlight-line))
      (,terminal-class (:foreground ,terminal-pitchkai-bg
                                    :background ,terminal-pitchkai-highlight-line))))

   `(term-color-red
     ((,class (:foreground ,pitchkai-red
                           :background ,pitchkai-red-d))
      (,terminal-class (:foreground ,terminal-pitchkai-red
                                    :background ,terminal-pitchkai-red-d))))

   `(term-color-green
     ((,class (:foreground ,pitchkai-green
                           :background ,pitchkai-green-d))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :background ,terminal-pitchkai-green-d))))

   `(term-color-yellow
     ((,class (:foreground ,pitchkai-yellow
                           :background ,pitchkai-yellow-d))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :background ,terminal-pitchkai-yellow-d))))

   `(term-color-blue
     ((,class (:foreground ,pitchkai-blue
                           :background ,pitchkai-blue-d))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :background ,terminal-pitchkai-blue-d))))

   `(term-color-magenta
     ((,class (:foreground ,pitchkai-magenta
                           :background ,pitchkai-magenta-d))
      (,terminal-class (:foreground ,terminal-pitchkai-magenta
                                    :background ,terminal-pitchkai-magenta-d))))

   `(term-color-cyan
     ((,class (:foreground ,pitchkai-cyan
                           :background ,pitchkai-cyan-d))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :background ,terminal-pitchkai-cyan-d))))

   `(term-color-white
     ((,class (:foreground ,pitchkai-emph
                           :background ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-emph
                                    :background ,terminal-pitchkai-fg))))

   `(term-default-fg-color
     ((,class (:inherit term-color-white))
      (,terminal-class (:inherit term-color-white))))

   `(term-default-bg-color
     ((,class (:inherit term-color-black))
      (,terminal-class (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,class (:background ,pitchkai-bg
                           :foreground ,pitchkai-yellow
                           :inherit ,s-variable-pitch))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,class (:foreground ,pitchkai-magenta
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-magenta
                                    :weight bold))))

   `(tuareg-font-lock-multistage-face
     ((,class (:foreground ,pitchkai-blue
                           :background ,pitchkai-highlight-line
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :background ,terminal-pitchkai-highlight-line
                                    :weight bold))))

   `(tuareg-font-lock-operator-face
     ((,class (:foreground ,pitchkai-emph))
      (,terminal-class (:foreground ,terminal-pitchkai-emph))))

   `(tuareg-font-lock-error-face
     ((,class (:foreground ,pitchkai-yellow
                           :background ,pitchkai-red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :background ,terminal-pitchkai-red
                                    :weight bold))))

   `(tuareg-font-lock-interactive-output-face
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   `(tuareg-font-lock-interactive-error-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,class (:foreground ,pitchkai-comments
                           :background ,pitchkai-bg))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :background ,terminal-pitchkai-bg))))

   `(undo-tree-visualizer-unmodified-face
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(undo-tree-visualizer-current-face
     ((,class (:foreground ,pitchkai-blue
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,class (:foreground ,pitchkai-emph
                           :background ,pitchkai-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-emph
                                    :background ,terminal-pitchkai-bg
                                    :weight bold))))

   `(undo-tree-visualizer-register-face
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   ;; volatile highlights
   `(vhl/default-face
     ((,class (:background ,pitchkai-green-lc
                           :foreground ,pitchkai-green-hc))
      (,terminal-class (:background ,terminal-pitchkai-green-lc
                                    :foreground ,terminal-pitchkai-green-hc))))

   ;; w3m
   `(w3m-anchor
     ((,class (:inherit link))
      (,terminal-class (:inherit link))))

   `(w3m-arrived-anchor
     ((,class (:inherit link-visited))
      (,terminal-class (:inherit link-visited))))

   `(w3m-form
     ((,class (:background ,pitchkai-bg
                           :foreground ,pitchkai-fg))
      (,terminal-class (:background ,terminal-pitchkai-bg
                                    :foreground ,terminal-pitchkai-fg))))

   `(w3m-header-line-location-title
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-yellow))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-yellow))))

   `(w3m-header-line-location-content

     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-fg))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-fg))))

   `(w3m-bold
     ((,class (:foreground ,pitchkai-emph
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-emph
                                    :weight bold))))

   `(w3m-image-anchor
     ((,class (:background ,pitchkai-bg
                           :foreground ,pitchkai-cyan
                           :inherit link))
      (,terminal-class (:background ,terminal-pitchkai-bg
                                    :foreground ,terminal-pitchkai-cyan
                                    :inherit link))))

   `(w3m-image
     ((,class (:background ,pitchkai-bg
                           :foreground ,pitchkai-cyan))
      (,terminal-class (:background ,terminal-pitchkai-bg
                                    :foreground ,terminal-pitchkai-cyan))))

   `(w3m-lnum-minibuffer-prompt
     ((,class (:foreground ,pitchkai-emph))
      (,terminal-class (:foreground ,terminal-pitchkai-emph))))

   `(w3m-lnum-match
     ((,class (:background ,pitchkai-highlight-line))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line))))

   `(w3m-lnum
     ((,class (:underline nil
                          :bold nil
                          :foreground ,pitchkai-red))
      (,terminal-class (:underline nil
                                   :bold nil
                                   :foreground ,terminal-pitchkai-red))))

   `(w3m-session-select
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(w3m-session-selected
     ((,class (:foreground ,pitchkai-emph
                           :bold t
                           :underline t))
      (,terminal-class (:foreground ,terminal-pitchkai-emph
                                    :bold t
                                    :underline t))))

   `(w3m-tab-background
     ((,class (:background ,pitchkai-bg
                           :foreground ,pitchkai-fg))
      (,terminal-class (:background ,terminal-pitchkai-bg
                                    :foreground ,terminal-pitchkai-fg))))

   `(w3m-tab-selected-background
     ((,class (:background ,pitchkai-bg
                           :foreground ,pitchkai-fg))
      (,terminal-class (:background ,terminal-pitchkai-bg
                                    :foreground ,terminal-pitchkai-fg))))

   `(w3m-tab-mouse
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-yellow))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-yellow))))

   `(w3m-tab-selected
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-emph
                           :bold t))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-emph
                                    :bold t))))

   `(w3m-tab-unselected
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-fg))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-fg))))

   `(w3m-tab-selected-retrieving
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-red))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-red))))

   `(w3m-tab-unselected-retrieving
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-orange))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-orange))))

   `(w3m-tab-unselected-unseen
     ((,class (:background ,pitchkai-highlight-line
                           :foreground ,pitchkai-violet))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :foreground ,terminal-pitchkai-violet))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(web-mode-comment-face
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   `(web-mode-constant-face
     ((,class (:foreground ,pitchkai-blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :weight bold))))

   `(web-mode-current-element-highlight-face
     ((,class (:underline unspecified
                          :weight unspecified
                          :background ,pitchkai-highlight-line))
      (,terminal-class (:underline unspecified
                                   :weight unspecified
                                   :background ,terminal-pitchkai-highlight-line))))

   `(web-mode-css-at-rule-face
     ((,class (:foreground ,pitchkai-violet
                           :slant italic))
      (,terminal-class (:foreground ,terminal-pitchkai-violet
                                    :slant italic))))

   `(web-mode-css-pseudo-class-face
     ((,class (:foreground ,pitchkai-green
                           :slant italic))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :slant italic))))

   `(web-mode-doctype-face
     ((,class (:foreground ,pitchkai-comments
                           :slant italic
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :slant italic
                                    :weight bold))))

   `(web-mode-folded-face
     ((,class (:underline t))
      (,terminal-class (:underline t))))

   `(web-mode-function-name-face
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(web-mode-html-attr-name-face
     ((,class (:foreground ,pitchkai-blue
                           :slant normal))
      (,terminal-class (:foreground ,terminal-pitchkai-blue
                                    :slant normal))))

   `(web-mode-html-attr-value-face
     ((,class (:foreground ,pitchkai-cyan
                           :slant italic))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan
                                    :slant italic))))

   `(web-mode-html-tag-face
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(web-mode-keyword-face
     ((,class (:foreground ,pitchkai-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :weight normal))))

   `(web-mode-preprocessor-face
     ((,class (:foreground ,pitchkai-yellow
                           :slant normal
                           :weight unspecified))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow
                                    :slant normal
                                    :weight unspecified))))

   `(web-mode-string-face
     ((,class (:foreground ,pitchkai-cyan))
      (,terminal-class (:foreground ,terminal-pitchkai-cyan))))

   `(web-mode-type-face
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(web-mode-variable-name-face
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(web-mode-warning-face
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(web-mode-block-attr-name-face
     ((,class (:inherit web-mode-html-attr-name-face))
      (,terminal-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-block-attr-value-face
     ((,class (:inherit web-mode-html-attr-value-face))
      (,terminal-class (:inherit web-mode-html-attr-value-face))))

   `(web-mode-block-comment-face
     ((,class (:inherit web-mode-comment-face))
      (,terminal-class (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,class (:inherit font-lock-preprocessor-face))
      (,terminal-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-face
     ((,class (:background unspecified))
      (,terminal-class (:background unspecified))))

   `(web-mode-block-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,class (:box 1
                    :weight bold))
      (,terminal-class (:box 1
                             :weight bold))))

   `(web-mode-css-color-face
     ((,class (:inherit font-lock-builtin-face))
      (,terminal-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-face
     ((,class (:inherit font-lock-builtin-face))
      (,terminal-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-priority-face
     ((,class (:inherit font-lock-builtin-face))
      (,terminal-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,class (:inherit font-lock-variable-name-face))
      (,terminal-class (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,class (:inherit font-lock-keyword-face))
      (,terminal-class (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-json-context-face
     ((,class (:foreground ,pitchkai-violet))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))

   `(web-mode-json-key-face
     ((,class (:foreground ,pitchkai-violet))
      (,terminal-class (:foreground ,terminal-pitchkai-violet))))

   `(web-mode-json-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(web-mode-part-comment-face
     ((,class (:inherit web-mode-comment-face))
      (,terminal-class (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,class (:inherit web-mode-block-face))
      (,terminal-class (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(web-mode-whitespace-face
     ((,class (:background ,pitchkai-red))
      (,terminal-class (:background ,terminal-pitchkai-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,class (:background unspecified
                           :foreground ,pitchkai-comments
                           :inverse-video unspecified
                           :slant italic))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-pitchkai-comments
                                    :inverse-video unspecified
                                    :slant italic))))

   `(whitespace-hspace
     ((,class (:background unspecified
                           :foreground ,pitchkai-emph
                           :inverse-video unspecified))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-pitchkai-emph
                                    :inverse-video unspecified))))

   `(whitespace-tab
     ((,class (:background unspecified
                           :foreground ,pitchkai-red
                           :inverse-video unspecified
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-pitchkai-red
                                    :inverse-video unspecified
                                    :weight bold))))

   `(whitespace-newline
     ((,class(:background unspecified
                          :foreground ,pitchkai-comments
                          :inverse-video unspecified))
      (,terminal-class(:background unspecified
                                   :foreground ,terminal-pitchkai-comments
                                   :inverse-video unspecified))))

   `(whitespace-trailing
     ((,class (:background unspecified
                           :foreground ,pitchkai-orange-lc
                           :inverse-video t))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-pitchkai-orange-lc
                                    :inverse-video t))))

   `(whitespace-line
     ((,class (:background unspecified
                           :foreground ,pitchkai-magenta
                           :inverse-video unspecified))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-pitchkai-magenta
                                    :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,class (:background ,pitchkai-red-lc
                           :foreground unspecified
                           :inverse-video unspecified))
      (,terminal-class (:background ,terminal-pitchkai-red-lc
                                    :foreground unspecified
                                    :inverse-video unspecified))))

   `(whitespace-indentation
     ((,class (:background unspecified
                           :foreground ,pitchkai-yellow
                           :inverse-video unspecified
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-pitchkai-yellow
                                    :inverse-video unspecified
                                    :weight bold))))

   `(whitespace-empty
     ((,class (:background unspecified
                           :foreground ,pitchkai-red-lc
                           :inverse-video t))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-pitchkai-red-lc
                                    :inverse-video t))))

   `(whitespace-space-after-tab
     ((,class (:background unspecified
                           :foreground ,pitchkai-orange
                           :inverse-video t
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-pitchkai-orange
                                    :inverse-video t
                                    :weight bold))))

   ;; wanderlust
   `(wl-highlight-folder-few-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(wl-highlight-folder-many-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(wl-highlight-folder-path-face
     ((,class (:foreground ,pitchkai-orange))
      (,terminal-class (:foreground ,terminal-pitchkai-orange))))

   `(wl-highlight-folder-unread-face
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(wl-highlight-folder-zero-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(wl-highlight-folder-unknown-face
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(wl-highlight-message-citation-header
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(wl-highlight-message-cited-text-1
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(wl-highlight-message-cited-text-2
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(wl-highlight-message-cited-text-3
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(wl-highlight-message-cited-text-4
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(wl-highlight-message-header-contents-face
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(wl-highlight-message-headers-face
     ((,class (:foreground ,pitchkai-red))
      (,terminal-class (:foreground ,terminal-pitchkai-red))))

   `(wl-highlight-message-important-header-contents
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(wl-highlight-message-header-contents
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(wl-highlight-message-important-header-contents2
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(wl-highlight-message-signature
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   `(wl-highlight-message-unimportant-header-contents
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(wl-highlight-summary-answepitchkai-red-face
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(wl-highlight-summary-disposed-face
     ((,class (:foreground ,pitchkai-fg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-pitchkai-fg
                                    :slant italic))))

   `(wl-highlight-summary-new-face
     ((,class (:foreground ,pitchkai-blue))
      (,terminal-class (:foreground ,terminal-pitchkai-blue))))

   `(wl-highlight-summary-normal-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(wl-highlight-summary-thread-top-face
     ((,class (:foreground ,pitchkai-yellow))
      (,terminal-class (:foreground ,terminal-pitchkai-yellow))))

   `(wl-highlight-thread-indent-face
     ((,class (:foreground ,pitchkai-magenta))
      (,terminal-class (:foreground ,terminal-pitchkai-magenta))))

   `(wl-highlight-summary-refiled-face
     ((,class (:foreground ,pitchkai-fg))
      (,terminal-class (:foreground ,terminal-pitchkai-fg))))

   `(wl-highlight-summary-displaying-face
     ((,class (:underline t
                          :weight bold))
      (,terminal-class (:underline t
                                   :weight bold))))

   ;; weechat
   `(weechat-error-face
     ((,class (:inherit error))
      (,terminal-class (:inherit error))))

   `(weechat-highlight-face
     ((,class (:foreground ,pitchkai-emph
                           :weight bold))
      (,terminal-class (:foreground ,terminal-pitchkai-emph
                                    :weight bold))))

   `(weechat-nick-self-face
     ((,class (:foreground ,pitchkai-green
                           :weight unspecified
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-pitchkai-green
                                    :weight unspecified
                                    :inverse-video t))))

   `(weechat-prompt-face
     ((,class (:inherit minibuffer-prompt))
      (,terminal-class (:inherit minibuffer-prompt))))

   `(weechat-time-face
     ((,class (:foreground ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments))))

   ;; which-func-mode
   `(which-func
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   ;; window-number-mode
   `(window-number-face
     ((,class (:foreground ,pitchkai-green))
      (,terminal-class (:foreground ,terminal-pitchkai-green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,class (:foreground ,pitchkai-comments
                           :background ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :background ,terminal-pitchkai-comments))))

   `(yascroll:thumb-fringe
     ((,class (:foreground ,pitchkai-comments
                           :background ,pitchkai-comments))
      (,terminal-class (:foreground ,terminal-pitchkai-comments
                                    :background ,terminal-pitchkai-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,class (:background ,pitchkai-highlight-line
                           :box ,pitchkai-emph))
      (,terminal-class (:background ,terminal-pitchkai-highlight-line
                                    :box ,terminal-pitchkai-emph)))))

  (custom-theme-set-variables
   'pitchkai
   `(ansi-color-names-vector [,pitchkai-bg ,pitchkai-red ,pitchkai-green ,pitchkai-yellow
                                           ,pitchkai-blue ,pitchkai-magenta ,pitchkai-cyan ,pitchkai-fg])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,pitchkai-highlight-line)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors '(,pitchkai-magenta ,pitchkai-violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,pitchkai-highlight-line . 0)
       (,pitchkai-green-lc . 20)
       (,pitchkai-cyan-lc . 30)
       (,pitchkai-blue-lc . 50)
       (,pitchkai-yellow-lc . 60)
       (,pitchkai-orange-lc . 70)
       (,pitchkai-magenta-lc . 85)
       (,pitchkai-highlight-line . 100)))

   ;; pos-tip
   `(pos-tip-foreground-color ,pitchkai-bg)
   `(pos-tip-background-color ,pitchkai-yellow)

   ;; vc
   `(vc-annotate-color-map
     '((20 . ,pitchkai-red)
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . ,pitchkai-yellow)
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . ,pitchkai-green)
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . ,pitchkai-cyan)
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . ,pitchkai-blue)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; weechat
   `(weechat-color-list
     (unspecified ,pitchkai-bg ,pitchkai-highlight-line
                  ,pitchkai-red-d ,pitchkai-red
                  ,pitchkai-green-d ,pitchkai-green
                  ,pitchkai-yellow-d ,pitchkai-yellow
                  ,pitchkai-blue-d ,pitchkai-blue
                  ,pitchkai-magenta-d ,pitchkai-magenta
                  ,pitchkai-cyan-d ,pitchkai-cyan
                  ,pitchkai-fg ,pitchkai-emph))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'pitchkai)

;; Local Variables:
;; no-byte-compile: t
;; fill-column: 95
;; End:

;;; pitchkai-theme.el ends here
