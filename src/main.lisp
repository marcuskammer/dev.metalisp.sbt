(defpackage cl-sbt
  (:use :cl)
  (:export
   :write-html-to-file
   :with-page
   :l10n
   :find-l10n))

(in-package :cl-sbt)

(setq spinneret:*fill-column* 120)
(defparameter *cdn-css* "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css")
(defparameter *cdn-js* "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js")

(defvar l10n '(("submit" ("en" "Submit" "de" "Absenden" "fr" "Soumettre"))
               ("cancel" ("en" "Cancel" "de" "Abbrechen" "fr" "Annuler"))
               ("upload" ("en" "Upload" "de" "Hochladen" "fr" "Télécharger"))
               ("search" ("en" "Search" "de" "Suchen" "fr" "Rechercher"))
               ("option-selected" ("en" "Open this selected menu"
                                   "de" "Das ausgewählte Menü öffnen"
                                   "fr" "Ouvrir le menu sélectionné"))
               ("sign-up" ("en" "Sign Up" "de" "Registrieren" "fr" "Inscrivez-vous"))
               ("sign-in" ("en" "Sign In" "de" "Anmelden" "fr" "S'identifier"))
               ("next" ("en" "Next" "de" "Weiter" "fr" "Suivant"))
               ("previous" ("en" "Previous" "de" "Zurück" "fr" "Précédent"))
               ("settings" ("en" "Settings" "de" "Einstellungen" "fr" "Paramètres"))
               ("logout" ("en" "Logout" "de" "Abmelden" "fr" "Déconnexion"))
               ("profile" ("en" "Profile" "de" "Profil" "fr" "Profil"))
               ("save" ("en" "Save" "de" "Speichern" "fr" "Enregistrer"))
               ("delete" ("en" "Delete" "de" "Löschen" "fr" "Supprimer"))
               ("edit" ("en" "Edit" "de" "Bearbeiten" "fr" "Modifier"))
               ("confirm" ("en" "Confirm" "de" "Bestätigen" "fr" "Confirmer"))
               ("loading" ("en" "Loading..." "de" "Lädt..." "fr" "Chargement..."))
               ("error" ("en" "Error" "de" "Fehler" "fr" "Erreur"))
               ("success" ("en" "Success" "de" "Erfolg" "fr" "Succès"))
               ("close" ("en" "Close" "de" "Schließen" "fr" "Fermer"))
               ("help" ("en" "Help" "de" "Hilfe" "fr" "Aide")))
  "Localization (l10n) settings for multi-language support.")

(defmacro with-page ((&key (author "") (description "") (cdn t) (pagetitle "") (theme "dark")) &body body)
  `(spinneret:with-html
     (:doctype)
     (:html :data-bs-theme ,theme
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:meta :name "author" :content ,author)
       (:meta :name "description" :content ,description)
       (:title ,pagetitle)
       (if ,cdn
           (:link :type "text/css" :rel "stylesheet" :href ,*cdn-css*)
           (:link :type "text/css" :rel "stylesheet" :href "5.3.0/bootstrap.min.css")))
      (:body (:h1 :class "visually-hidden" ,pagetitle)
        (:main ,@body))
      (if ,cdn
          (:script :src *cdn-js*)
          (:script :src "5.3.0/bootstrap.bundle.min.js")))))

(defun write-html-to-file (filename string &key (lang "en") (style :tree) (fc 120))
  (let ((spinneret:*html-lang* lang)
        (spinneret:*html-style* style)
        (spinneret:*fill-column* fc))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (write-string string stream))))

(defun find-l10n (key lang alist)
  "Finds the localized string for a given key and language.

KEY: The key to look up the localization for.

LANG: The language to get the localized string for."
  (cadr (member lang (cadr (assoc key alist :test #'string=)) :test #'string=)))
