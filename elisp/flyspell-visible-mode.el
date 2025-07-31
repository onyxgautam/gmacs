;;; flyspell-visible-mode.el --- spell check visible on screen text.

;; Copyright (C) 2019  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://github.com/ideasman42/emacs-flyspell-visible-mode
;; Version: 0.1
;; Package-Requires: ((emacs "26.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Ensures spell checking runs for on-screen text,
;; using a delay so as not to impact performance.

;;; Usage

;; (flyspell-visible-mode)        ; activate in the current buffer

;;; Code:

(defcustom flyspell-visible-delay 0.5
  "Idle time before checking spelling."
  :group 'flyspell-visible-mode
  :type  'float)

(defcustom flyspell-visible-mode-lighter ""
  "Lighter for flyspell-visible-mode."
  :group 'flyspell-visible-mode
  :type 'string)

(defun flyspell-visible--visible-region (buf)
  (when (equal buf (window-buffer))
    (let* ((pt-min (point-min))
           (pt-max (point-max))
           (range-start (max (window-start) pt-min))
           (range-end (min (window-end) pt-max))
           (ranges (list range-start range-end pt-min pt-max)))
      (when (not (equal ranges flyspell-visible--last-window-ranges))
        (let ((inhibit-quit t)
              (inhibit-redisplay t))
          (if (with-local-quit
                (save-excursion
                  (flyspell-large-region range-start range-end))
                t)
              (setq-local flyspell-visible--last-window-ranges ranges)
            (setq-local flyspell-visible--last-window-ranges nil))
          ;; TODO, investigate why this is needed to avoid locking up
          ;; only when 'flyspell-prog-mode' is off, very odd.
          ;; (let ((inhibit-message t))
          ;;   (signal 'quit nil))
          )))))

(defvar-local flyspell-visible--delay-timer nil "Buffer-local timer.")
(defvar-local flyspell-visible--last-window-ranges nil)

(defun flyspell-visible--delay-timer-setup ()
  "Recalculate overlays using a delay (to avoid slow-down)."
  (when (timerp flyspell-visible--delay-timer)
    (cancel-timer flyspell-visible--delay-timer))
  (setq flyspell-visible--delay-timer
        (run-with-idle-timer
         flyspell-visible-delay t
         'flyspell-visible--visible-region (current-buffer))))

(defun flyspell-visible-mode-enable ()
  "Turn on 'flyspell-visible-mode' for the current buffer."
  (add-hook 'kill-buffer-hook 'flyspell-visible-mode-disable nil t)
  (flyspell-visible--delay-timer-setup))

(defun flyspell-visible-mode-disable ()
  "Turn off 'flyspell-visible-mode' for the current buffer."
  (when (timerp flyspell-visible--delay-timer)
    (cancel-timer flyspell-visible--delay-timer)))

;;;###autoload
(define-minor-mode flyspell-visible-mode
  "Highlight block under the cursor."
  :global nil
  :lighter flyspell-visible-mode-lighter
  (cond (flyspell-visible-mode
         (jit-lock-unregister 'flyspell-visible-mode-enable)
         (flyspell-visible-mode-enable))
        (t
         (jit-lock-unregister 'flyspell-visible-mode-enable)
         (flyspell-visible-mode-disable))))

(provide 'flyspell-visible-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; flyspell-visible-mode.el ends here
