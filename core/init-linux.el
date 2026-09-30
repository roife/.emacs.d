;;; -*- lexical-binding: t -*-

;; Follow KDE, GNOME, and other desktops exposing the XDG Settings portal.
(defvar +linux-appearance-signal nil
  "D-Bus registration for desktop appearance changes.")

(defun +linux-appearance-changed (namespace key value)
  "Handle a Settings portal change to NAMESPACE, KEY, and VALUE."
  (when (and (equal namespace "org.freedesktop.appearance")
             (equal key "color-scheme"))
    ;; Read wraps the value twice; SettingChanged wraps it once.
    (while (consp value)
      (setq value (car value)))
    ;; Keep the existing dark default when the desktop has no preference.
    (+system-appearance-changed (if (eq value 2) 'light 'dark))))

(add-hook! 'after-make-frame-functions :call-immediately
  (defun +linux-setup-appearance (&optional _frame)
    "Subscribe to desktop appearance changes and read the current setting."
    (when (and (require 'dbus nil t) (fboundp 'dbus-register-signal))
      ;; Headless sessions and builds without a session bus still start normally.
      ;; Retry on frame creation, which also refreshes the daemon's initial theme.
      (condition-case nil
          (progn
            (unless +linux-appearance-signal
              (setq +linux-appearance-signal
                    (dbus-register-signal
                     :session "org.freedesktop.portal.Desktop"
                     "/org/freedesktop/portal/desktop"
                     "org.freedesktop.portal.Settings" "SettingChanged"
                     #'+linux-appearance-changed)))
            (+linux-appearance-changed
             "org.freedesktop.appearance" "color-scheme"
             (dbus-call-method
              :session "org.freedesktop.portal.Desktop"
              "/org/freedesktop/portal/desktop"
              "org.freedesktop.portal.Settings" "Read" :timeout 1000
              "org.freedesktop.appearance" "color-scheme")))
        (dbus-error nil)))))
