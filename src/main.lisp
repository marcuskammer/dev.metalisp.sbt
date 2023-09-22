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

(defun dictp (lst)
  (loop for entry in lst always
        (and (listp entry)
             (= (length entry) 2)
             (stringp (first entry))
             (every #'stringp (second entry)))))

(deftype dict ()
  '(and list (satisfies dictp)))

(declaim (type dict *l10n*))
(defparameter *l10n*
  '(("submit" ("en" "Submit" "de" "Absenden" "fr" "Soumettre"))
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
    ("help" ("en" "Help" "de" "Hilfe" "fr" "Aide"))
    ("home" ("en" "Home" "de" "Startseite" "fr" "Accueil"))
    ("welcome" ("en" "Welcome" "de" "Willkommen" "fr" "Bienvenue"))
    ("faq" ("en" "FAQ" "de" "Häufig gestellte Fragen" "fr" "FAQ"))
    ("contact" ("en" "Contact" "de" "Kontakt" "fr" "Contact"))
    ("privacy" ("en" "Privacy" "de" "Datenschutz" "fr" "Confidentialité"))
    ("terms" ("en" "Terms and Conditions" "de" "Allgemeine Geschäftsbedingungen" "fr" "Conditions Générales"))
    ("about" ("en" "About Us" "de" "Über uns" "fr" "À propos de nous"))
    ("add-to-cart" ("en" "Add to Cart" "de" "In den Warenkorb" "fr" "Ajouter au panier"))
    ("checkout" ("en" "Checkout" "de" "Kasse" "fr" "Passer à la caisse"))
    ("forgot-password" ("en" "Forgot Password?" "de" "Passwort vergessen?" "fr" "Mot de passe oublié ?"))
    ("username" ("en" "Username" "de" "Benutzername" "fr" "Nom d'utilisateur"))
    ("password" ("en" "Password" "de" "Passwort" "fr" "Mot de passe"))
    ("email" ("en" "Email" "de" "E-Mail" "fr" "Courrier électronique"))
    ("language" ("en" "Language" "de" "Sprache" "fr" "Langue"))
    ("read-more" ("en" "Read More" "de" "Weiterlesen" "fr" "En savoir plus"))
    ("show-less" ("en" "Show Less" "de" "Weniger anzeigen" "fr" "Montrer moins"))
    ("update" ("en" "Update" "de" "Aktualisieren" "fr" "Mettre à jour"))
    ("new" ("en" "New" "de" "Neu" "fr" "Nouveau"))
    ("old" ("en" "Old" "de" "Alt" "fr" "Ancien"))
    ("view-all" ("en" "View All" "de" "Alle anzeigen" "fr" "Voir tout"))
    ("cart" ("en" "Cart" "de" "Warenkorb" "fr" "Panier"))
    ("favorites" ("en" "Favorites" "de" "Favoriten" "fr" "Favoris"))
    ("share" ("en" "Share" "de" "Teilen" "fr" "Partager"))
    ("download" ("en" "Download" "de" "Herunterladen" "fr" "Télécharger"))
    ("print" ("en" "Print" "de" "Drucken" "fr" "Imprimer"))
    ("back" ("en" "Back" "de" "Zurück" "fr" "Retour"))
    ("create-account" ("en" "Create Account" "de" "Konto erstellen" "fr" "Créer un compte"))
    ("learn-more" ("en" "Learn More" "de" "Mehr erfahren" "fr" "En savoir plus")))
  "Localization (l10n) settings for multi-language support.")

(declaim (ftype (function (string string dict) string) find-l10n))
(defun find-l10n (key lang dict)
  "Finds the localized string for a given key and language."
  (let ((entry (cadr (assoc key dict :test #'string=))))
    (if entry
        (let ((term (cadr (member lang entry :test #'string=))))
          (or term "Translation not found"))
        "Key not found")))

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
