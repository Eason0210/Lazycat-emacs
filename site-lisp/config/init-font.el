;;; init-font.el --- Font configuration

;; Filename: init-font.el
;; Description: Font configuration
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2020, Andy Stewart, all rights reserved.
;; Created: 2020-03-22 11:16:26
;; Version: 0.1
;; Last-Updated: 2020-03-22 11:16:26
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-font.el
;; Keywords:
;; Compatibility: GNU Emacs 26.3
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Font configuration
;;

;;; Installation:
;;
;; Put init-font.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-font)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-font RET
;;

;;; Change log:
;;
;; 2020/03/22
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(defconst *is-a-win64* (eq system-type 'windows-nt))
(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-a-mac* (eq system-type 'darwin))

(when *is-a-mac* 

(let ((emacs-font-size 14)
      emacs-font-name)
  (cond
   ((featurep 'cocoa)
    (setq emacs-font-name "Monaco"))
   ((string-equal system-type "gnu/linux")
    (setq emacs-font-name "WenQuanYi Micro Hei Mono")))
  (when (display-grayscale-p)
    (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
    (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name))))

(with-eval-after-load 'org
  (defun org-buffer-face-mode-variable ()
    (interactive)
    (make-face 'width-font-face)
    (set-face-attribute 'width-font-face nil :font "等距更纱黑体 SC 15")
    (setq buffer-face-mode-face 'width-font-face)
    (buffer-face-mode))

  (add-hook 'org-mode-hook 'org-buffer-face-mode-variable))

	)

(when *is-a-win64*
  ;;font setting
  (setq fonts '("Inconsolata Bold" "华文楷体"))
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (car fonts) 20))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family (car (cdr fonts)))))
  (setq face-font-rescale-alist '("华文楷体" . 1.0))


  (setq w32-apps-modifier 'control)
)


;;(when *is-a-linux*
  ;;(setq fonts '("Inconsolata" "STKaiti"))
  ;;(set-face-attribute 'default nil :font
                      ;;(format "%s:pixelsize=%d" (car fonts) 18))
  ;;(dolist (charset '(kana han symbol cjk-misc bopomofo))
    ;;(set-fontset-font (frame-parameter nil 'font) charset
                      ;;(font-spec :family (car (cdr fonts)))))
  ;;(setq face-font-rescale-alist '("STKaiti" . 1.0)))


(setq-default mode-line-format (remove 'mode-line-buffer-identification mode-line-format))

(provide 'init-font)

;;; init-font.el ends here
