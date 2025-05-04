           IDENTIFICATION DIVISION.
           PROGRAM-ID. Exocalc.
           AUTHOR SIBORY G.
      *    Calculette 
           

           ENVIRONMENT DIVISION.
           
           DATA DIVISION.
           WORKING-STORAGE SECTION.

      *    Les variables pour afficher les résultats
       01  WS-NBR1 PIC Z(3)9.99.
       01  WS-NBR2 PIC Z(3)9.99.
       01  WS-RESULT1 PIC Z(6)9.99.
       01  WS-RESULT2 PIC Z(6)9.99.

      *    Les variables pour effectuer les calculs
       01  WS-CALC-NBR1 PIC S9(6)V99.
       01  WS-CALC-NBR2 PIC S9(6)V99.
       01  WS-CALC-RESULT1 PIC S9(6)V99.

      *    Les variables pour les calculs en fonction 
      *    du choix de l'opération
       01  WS-CALC-ADD PIC S9(3)V99.
       01  WS-CALC-SOUS PIC S9(3)V99.
       01  WS-CALC-MULTI PIC S9(3)V99.
       01  WS-CALC-DIV PIC S9(3)V99.
       01  WS-DIV-RESULT PIC S9(3)V99.
       01  WS-CALC-XP PIC S9(6)V99.

      *    Les variables pour les diverses intéractions avec le menu
       01  WS-CHOIX-MENU PIC 9(1).
       01  WS-CHOIX-OPE PIC 9(1).
       01  WS-MESS PIC X(18) VALUE "Erreur de saisie !".
       01  WS-REP PIC X(1).       
       01  WS-CALC-QUIT PIC X(1).

           PROCEDURE DIVISION.           

           PERFORM CALCULETTE
           STOP RUN.

      *    Paragraphe pour afficher constamment le menu principal
           CALCULETTE.
           PERFORM AFFICHAGE_MENU.

      *    Paragraphe pour afficher le menu et quitter
           AFFICHAGE_MENU.
           DISPLAY "1 - Calcul  2 - Quitter".
           ACCEPT WS-CHOIX-MENU.

           IF WS-CHOIX-MENU = 1 THEN 
                PERFORM PREMIER_NBR
           ELSE IF WS-CHOIX-MENU = 2
                PERFORM QUITTER-CALC
           ELSE 
                PERFORM MESSAGE-ERREUR
                PERFORM AFFICHAGE_MENU
           END-IF.   
           
      *    Paragraphe pour saisir le premier nombre des opérations 
           PREMIER_NBR.
           DISPLAY "Veuillez saisir un nombre :".
           ACCEPT WS-CALC-NBR1.
           PERFORM AFFICHER_CHOIX_OPE.

      *    Paragraphe pour l'affichage du choix de l'opération
           AFFICHER_CHOIX_OPE.
           DISPLAY "......................................".
           DISPLAY "1 - Additionner         2 - Soustraire".
           DISPLAY "3 - Multiplier          4 - Diviser".
           DISPLAY "5 - Puissance           6 - Retour".
           DISPLAY "......................................".
           ACCEPT WS-CHOIX-OPE.
           
           EVALUATE WS-CHOIX-OPE
               WHEN 1
                    PERFORM ADDITIONNER               
      
               WHEN 2
                    PERFORM SOUSTRAIRE
      
               WHEN 3
                    PERFORM MULTIPLIER
      
               WHEN 4
                    PERFORM DIVISER
      
               WHEN 5
                    PERFORM EXPOSER

               WHEN 6
                    PERFORM RETOUR_CALC

               WHEN OTHER
                    PERFORM MESSAGE-ERREUR

           END-EVALUATE.

      *    Paragraphe pour les messages d'erreur
           MESSAGE-ERREUR.
           DISPLAY WS-MESS.

      *    Paragraphe pour revenir au menu principal
           RETOUR_CALC.
           PERFORM AFFICHAGE_MENU.

      *    Paragraphe pour continuer
           CONTINUER_CALC.
           DISPLAY "Continuer ? (Y/N)".
           ACCEPT WS-REP.
           EVALUATE WS-REP
                WHEN "y"
                     MOVE WS-CALC-RESULT1 TO WS-CALC-NBR1
                     PERFORM AFFICHER_CHOIX_OPE
                WHEN "Y"
                     MOVE WS-CALC-RESULT1 TO WS-CALC-NBR1
                     PERFORM AFFICHER_CHOIX_OPE
                WHEN "n"
                     MOVE 0 TO WS-CALC-NBR1
                     PERFORM AFFICHAGE_MENU
                WHEN "N"
                     MOVE 0 TO WS-CALC-NBR1
                     PERFORM AFFICHAGE_MENU
                WHEN OTHER
                     PERFORM MESSAGE-ERREUR
           END-EVALUATE.
      
      *    Paragraphe pour quitter la calculette
           QUITTER-CALC.
           DISPLAY "Voulez-vous quitter ? (Y/N)".
           ACCEPT WS-CALC-QUIT.      
           EVALUATE WS-CALC-QUIT
               WHEN "y"
               STOP RUN

               WHEN "Y"
               STOP RUN

               WHEN "n"
               PERFORM CALCULETTE

               WHEN "N"
               PERFORM CALCULETTE

               WHEN OTHER
               PERFORM MESSAGE-ERREUR
               PERFORM QUITTER-CALC
           END-EVALUATE.

      *    Paragraphe pour l'addition
           ADDITIONNER.
           MOVE WS-CALC-NBR1 TO WS-NBR1.
           DISPLAY "Saisissez un nouveau nombre à additionner:".
           ACCEPT WS-CALC-ADD.
           MOVE WS-CALC-ADD TO WS-NBR2.
           DISPLAY "Voulez-vous le détail de calcul ? (Y/N)".
           ACCEPT WS-REP.

           EVALUATE WS-REP 
                WHEN "y"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 + WS-CALC-ADD
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM CALC-SESSION
                PERFORM CONTINUER_CALC

                WHEN "Y"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 + WS-CALC-ADD
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM CALC-SESSION
                PERFORM CONTINUER_CALC

                WHEN "n"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 + WS-CALC-ADD
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM RESULT
                PERFORM CONTINUER_CALC

                WHEN "N"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 + WS-CALC-ADD
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM RESULT
                PERFORM CONTINUER_CALC

                WHEN OTHER 
                PERFORM MESSAGE-ERREUR
           END-EVALUATE.

      *    Paragraphe pour la soustraction
           SOUSTRAIRE.
           MOVE WS-CALC-NBR1 TO WS-NBR1.
           DISPLAY "Saisissez un nouveau nombre à soustraire:".
           ACCEPT WS-CALC-SOUS.
           MOVE WS-CALC-SOUS TO WS-NBR2.
           DISPLAY "Voulez-vous le détail de calcul ? (Y/N)"
           ACCEPT WS-REP.

           EVALUATE WS-REP 
                WHEN "y"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 - WS-CALC-SOUS
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM CALC-SESSION
                PERFORM CONTINUER_CALC

                WHEN "Y"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 - WS-CALC-SOUS
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM CALC-SESSION
                PERFORM CONTINUER_CALC

                WHEN "n"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 - WS-CALC-SOUS
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM RESULT
                PERFORM CONTINUER_CALC

                WHEN "N"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 - WS-CALC-SOUS
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM RESULT
                PERFORM CONTINUER_CALC

                WHEN OTHER 
                PERFORM MESSAGE-ERREUR
           END-EVALUATE.

      *    Paragraphe pour la multiplication
           MULTIPLIER.
           MOVE WS-CALC-NBR1 TO WS-NBR1.
           DISPLAY "Saisissez un nouveau nombre à multiplier :".
           ACCEPT WS-CALC-MULTI.
           MOVE WS-CALC-MULTI TO WS-NBR2.
           DISPLAY "Voulez-vous le détail de calcul ? (Y/N)".
           ACCEPT WS-REP.

           EVALUATE WS-REP 
                WHEN "y"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 * WS-CALC-MULTI
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM CALC-SESSION
                PERFORM CONTINUER_CALC

                WHEN "Y"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 * WS-CALC-MULTI
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM CALC-SESSION
                PERFORM CONTINUER_CALC

                WHEN "n"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 * WS-CALC-MULTI
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM RESULT
                PERFORM CONTINUER_CALC

                WHEN "N"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 * WS-CALC-MULTI
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM RESULT
                PERFORM CONTINUER_CALC

                WHEN OTHER 
                PERFORM MESSAGE-ERREUR
           END-EVALUATE.

      *    Paragraphe pour la division
           DIVISER.
           DISPLAY "Saisissez un diviseur :".
           ACCEPT WS-CALC-DIV.

           IF WS-CALC-DIV = 0
                THEN DISPLAY "Erreur ! Division par 0 impossible !"
                PERFORM DIVISER
           ELSE IF WS-NBR1 = 0
                THEN DISPLAY "On ne peut diviser 0 !"
           ELSE
                MOVE WS-CALC-NBR1 TO WS-NBR1
                MOVE WS-CALC-DIV TO WS-NBR2
                DISPLAY "Voulez-vous le détail de calcul ? (Y/N)"
                ACCEPT WS-REP

           EVALUATE WS-REP
                WHEN "y"
                MOVE WS-CALC-NBR1 TO WS-NBR1
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 / WS-CALC-DIV
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM CALC-SESSION
                PERFORM CONTINUER_CALC

                WHEN "Y"
                MOVE WS-CALC-NBR1 TO WS-NBR1
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 / WS-CALC-DIV
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM CALC-SESSION
                PERFORM CONTINUER_CALC

                WHEN "n"
                MOVE WS-CALC-NBR1 TO WS-NBR1
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 / WS-CALC-DIV
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM RESULT
                PERFORM CONTINUER_CALC

                WHEN "N"
                MOVE WS-CALC-NBR1 TO WS-NBR1
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 / WS-CALC-DIV
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM RESULT
                PERFORM CONTINUER_CALC           

                WHEN OTHER
                PERFORM MESSAGE-ERREUR
           END-EVALUATE
           END-IF.

      *    Paragraphe pour les expopsants
           EXPOSER.
           MOVE WS-CALC-NBR1 TO WS-NBR1.
           DISPLAY "Saisissez un exposant".
           ACCEPT WS-CALC-XP.
           MOVE WS-CALC-XP TO WS-NBR2.
           DISPLAY "Voulez-vous le détail de calcul ? (Y/N)"
           ACCEPT WS-REP.

           EVALUATE WS-REP
                WHEN "y"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 ** WS-CALC-XP
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM CALC-SESSION
                PERFORM CONTINUER_CALC

           WHEN "Y"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 ** WS-CALC-XP
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM CALC-SESSION
                PERFORM CONTINUER_CALC

           WHEN "n"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 ** WS-CALC-XP
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM RESULT
                PERFORM CONTINUER_CALC

           WHEN "N"
                COMPUTE WS-CALC-NBR1 = WS-CALC-NBR1 ** WS-CALC-XP
                MOVE WS-CALC-NBR1 TO WS-CALC-RESULT1
                PERFORM RESULT
                PERFORM CONTINUER_CALC

                WHEN OTHER 
                PERFORM MESSAGE-ERREUR
           END-EVALUATE.

      *    Paragraphe pour le résultat sans le détail
           RESULT.
           MOVE WS-CALC-RESULT1 TO WS-RESULT1
           IF WS-CALC-RESULT1 < 0 THEN
                DISPLAY "Résultat = -" FUNCTION TRIM(WS-RESULT1)
           ELSE
                DISPLAY "Résultat = " FUNCTION TRIM(WS-RESULT1)
           END-IF.

      *    Paragraphe pour le détail des calculs
           CALC-SESSION.
           EVALUATE WS-CHOIX-OPE
                WHEN 1
                MOVE WS-CALC-RESULT1 TO WS-RESULT1
      
                     IF WS-CALC-RESULT1 > 0 THEN
                          DISPLAY "Résultat : "
                          FUNCTION TRIM(WS-NBR1) 
                          " + " 
                          FUNCTION TRIM(WS-NBR2) 
                          " = " 
                          FUNCTION TRIM(WS-RESULT1)
                     ELSE
                          DISPLAY "Résultat : "
                          FUNCTION TRIM(WS-NBR1) 
                          " + " 
                          FUNCTION TRIM(WS-NBR2) 
                          " = -" 
                          FUNCTION TRIM(WS-RESULT1)
                     END-IF
      
                WHEN 2
                MOVE WS-CALC-RESULT1 TO WS-RESULT1
      
                     IF WS-CALC-NBR1 < 0 THEN
                          DISPLAY "Résultat : -"
                          FUNCTION TRIM(WS-NBR1) 
                          " - " 
                          FUNCTION TRIM(WS-NBR2) 
                          " = " 
                          FUNCTION TRIM(WS-RESULT1)
                     ELSE IF WS-CALC-RESULT1 > 0 THEN
                          DISPLAY "Résultat : "
                          FUNCTION TRIM(WS-NBR1) 
                          " - " 
                          FUNCTION TRIM(WS-NBR2) 
                          " = " 
                          FUNCTION TRIM(WS-RESULT1)
                     ELSE
                          DISPLAY "Résultat : "
                          FUNCTION TRIM(WS-NBR1) 
                          " - " 
                          FUNCTION TRIM(WS-NBR2) 
                          " = -" 
                          FUNCTION TRIM(WS-RESULT1)
                     END-IF
      
                WHEN 3
                MOVE WS-CALC-RESULT1 TO WS-RESULT1
      
                     IF WS-CALC-RESULT1 > 0 THEN
                          DISPLAY "Résultat : "
                          FUNCTION TRIM(WS-NBR1)
                          " x "
                          FUNCTION TRIM(WS-NBR2)
                          " = "
                          FUNCTION TRIM(WS-RESULT1)
                     ELSE
                          DISPLAY "Résultat : "
                          FUNCTION TRIM(WS-NBR1)
                          " x "
                          FUNCTION TRIM(WS-NBR2)
                          " = -"
                          FUNCTION TRIM(WS-RESULT1)
                     END-IF

                WHEN 4
                MOVE WS-CALC-RESULT1 TO WS-RESULT1
      
                     IF WS-CALC-RESULT1 > 0 THEN
                     DISPLAY "Résultat : "
                     FUNCTION TRIM(WS-NBR1)
                     " / "
                     FUNCTION TRIM(WS-NBR2)
                     " = "
                     FUNCTION TRIM(WS-RESULT1)
                     ELSE
                     DISPLAY "Résultat : "
                     FUNCTION TRIM(WS-NBR1)
                     " / "
                     FUNCTION TRIM(WS-NBR2)
                     " = "
                     FUNCTION TRIM(WS-RESULT1)
                     END-IF

                WHEN 5
                MOVE WS-CALC-RESULT1 TO WS-RESULT1
      
                     IF WS-CALC-RESULT1 > 0 THEN
                     DISPLAY "Résultat : "
                     FUNCTION TRIM(WS-NBR1)
                     " ^ "
                     FUNCTION TRIM(WS-NBR2)
                     " = "
                     FUNCTION TRIM(WS-RESULT1)
                     ELSE
                     DISPLAY "Résultat : "
                     FUNCTION TRIM(WS-NBR1)
                     " ^ "
                     FUNCTION TRIM(WS-NBR2)
                     " = "
                     FUNCTION TRIM(WS-RESULT1)
                     END-IF
           END-EVALUATE.
