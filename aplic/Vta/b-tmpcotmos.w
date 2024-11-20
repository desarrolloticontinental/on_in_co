= 16.77
         VIRTUAL-WIDTH      = 88.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

IF NOT W-Win:LOAD-ICON("img/plobrper":U) THEN
    MESSAGE "Unable to load icon: img/plobrper"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   Default                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Asignaci�n de Personal - Obreros */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Asignaci�n de Personal - Obreros */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Datos|Personal|Cargos|CTS|Secciones|Clases|Proyectos|Canales' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 16.54 , 85.86 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'pln/q-plan-s.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-plan-s ).
       /* Position in UIB:  ( 2.19 , 2.57 ) */
       /* Size in UIB:  ( 1.31 , 5.43 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'pln/b-pers-s.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-pers-s ).
       RUN set-position IN h_b-pers-s ( 3.81 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.65 , 68.29 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'pln/v-pers-s.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-pers-s ).
       RUN set-position IN h_v-pers-s ( 8.69 , 2.86 ) NO-ERROR.
       /* Size in UIB:  ( 8.35 , 82.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'pln/v-plan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-plan ).
       RUN set-position IN h_v-plan ( 2.27 , 8.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.35 , 52.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 2.27 , 60.57 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 1.38 , 18.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updva2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updva2 ).
       RUN set-position IN h_p-updva2 ( 3.81 , 72.57 ) NO-ERROR.
       RUN set-size IN h_p-updva2 ( 4.38 , 12.00 ) NO-ERROR.

       /* Links to SmartQuery h_q-plan-s. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_q-plan-s ).

       /* Links to SmartBrowser h_b-pers-s. */
       RUN add-link IN adm-broker-hdl ( h_v-plan , 'Record':U , h_b-pers-s ).

       /* Links to SmartViewer h_v-pers-s. */
       RUN add-link IN adm-broker-hdl ( h_b-pers-s , 'Record':U , h_v-pers-s ).
       RUN add-link IN adm-broker-hdl ( h_p-updva2 , 'TableIO':U , h_v-pers-s ).

       /* Links to SmartViewer h_v-plan. */
       RUN add-link IN adm-broker-hdl ( h_q-plan-s , 'Record':U , h_v-plan ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'PLN/V-PERS1.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_V-PERS1 ).
       RUN set-position IN h_V-PERS1 ( 4.38 , 1.86 ) NO-ERROR.
       /* Size in UIB:  ( 10.65 , 84.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updpar.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updpar ).
       RUN set-position IN h_p-updpar ( 15.27 , 34.00 ) NO-ERROR.
       RUN set-size IN h_p-updpar ( 1.77 , 20.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartViewer h_V-PERS1. */
       RUN add-link IN adm-broker-hdl ( h_b-pers-s , 'Record':U , h_V-PERS1 ).
       RUN add-link IN adm-broker-hdl ( h_p-updpar , 'TableIO':U , h_V-PERS1 ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'pln/b-carg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-carg ).
       RUN set-position IN h_b-carg ( 4.27 , 10.57 ) NO-ERROR.
       /* Size in UIB:  ( 9.88 , 50.29 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav ).
       RUN set-position IN h_p-updsav ( 4.27 , 61.43 ) NO-ERROR.
       RUN set-size IN h_p-updsav ( 9.85 , 10.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-carg. */
       RUN add-link IN adm-broker-hdl ( h_p-updsav , 'TableIO':U , h_b-carg ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'pln/b-cts.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-cts ).
       RUN set-position IN h_b-cts ( 4.15 , 17.43 ) NO-ERROR.
       /* Size in UIB:  ( 8.65 , 42.14 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-2 ).
       RUN set-position IN h_p-updsav-2 ( 4.15 , 60.29 ) NO-ERROR.
       RUN set-size IN h_p-updsav-2 ( 8.65 , 10.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-cts. */
       RUN add-link IN adm-broker-hdl ( h_p-updsav-2 , 'TableIO':U , h_b-cts ).

    END. /* Page 4 */

    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'PLN/B-SECC.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_B-SECC ).
       RUN set-position IN h_B-SECC ( 4.42 , 13.29 ) NO-ERROR.
       /* Size in UIB:  ( 8.62 , 48.14 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-5 ).
       RUN set-position IN h_p-updsav-5 ( 4.42 , 62.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav-5 ( 8.62 , 10.00 ) NO-ERROR.

       /* Links to SmartBrowser h_B-SECC. */
       RUN add-link IN adm-broker-hdl ( h_p-updsav-5 , 'TableIO':U , h_B-SECC ).

    END. /* Page 5 */

    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'pln/b-clas.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-clas ).
       RUN set-position IN h_b-clas ( 5.15 , 4.14 ) NO-ERROR.
       /* Size in UIB:  ( 8.65 , 64.29 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-3 ).
       RUN set-position IN h_p-updsav-3 ( 5.15 , 69.57 ) NO-ERROR.
       RUN set-size IN h_p-updsav-3 ( 8.65 , 10.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-clas. */
       RUN add-link IN adm-broker-hdl ( h_p-updsav-3 , 'TableIO':U , h_b-clas ).

    END. /* Page 6 */

    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'pln/b-proy.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-proy ).
       RUN set-position IN h_b-proy ( 5.35 , 7.29 ) NO-ERROR.
       /* Size in UIB:  ( 7.85 , 60.14 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-4 ).
       RUN set-position IN h_p-updsav-4 ( 5.35 , 68.14 ) NO-ERROR.
       RUN set-size IN h_p-updsav-4 ( 7.85 , 10.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-proy. */
       RUN add-link IN adm-broker-hdl ( h_p-updsav-4 , 'TableIO':U , h_b-proy ).

    END. /* Page 7 */

    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'PLN/B-PAGO.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_B-PAGO ).
       RUN set-position IN h_B-PAGO ( 4.77 , 11.43 ) NO-ERROR.
       /* Size in UIB:  ( 9.23 , 49.43 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-6 ).
       RUN set-position IN h_p-updsav-6 ( 4.77 , 61.43 ) NO-ERROR.
       RUN set-size IN h_p-updsav-6 ( 9.23 , 10.00 ) NO-ERROR.

       /* Links to SmartBrowser h_B-PAGO. */
       RUN add-link IN adm-broker-hdl ( h_p-updsav-6 , 'TableIO':U , h_B-PAGO ).

    END. /* Page 8 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  VIEW FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             	��V�˸:    8�4              a                                V}      `  �              2o  `t�"      $  ) �4  �  �5  �  L6  0 |7  �  P8   X9  �  ,:  �  �:  �  �;  �	 8=  � 
 �=  �  �>  ( �?   �@  X TB   hC  8 �D   �E  � �I  �  `J   dK  � $M   (N   ,O   0P  P �^  � c  �  �c  �  �d  �   \e  � ! �e  � "         �f  �   ? �g  �iSO8859-1                        �  $ �                       T                             F�                                                      PROGRESS                           ,    � 
   
                �        �                                             ,      
  > ��  P       > �� T       ��                           l       ����                          h     H 8                    X      ��x  O ��    e�      O ��    R�      O ��    ��    t        � $ ( � ���                         � ߱  H i � �   4 ��   o j    �                   �< NAP �X �l  �  � � � � � � `
` $4H \  $ { ,���        p   
            � ߱  � } T\    4 ���  	~ x                    3 ���� � ��    4 ���  $ � ����        �@        �       � ߱     
            � ߱  �$ a����        (/	o   8          3 ��            3 ��D  p4t    4 ��X        |          ��             qt       �Rv   q<�/r�                3 ��l�$ r����        �   
              � ߱    /s�   ��          3 ���   �        3 ����$ y���        �@        �       � ߱  adm-apply-entry   0                �                     � adm-destroy @p                �                     � adm-disable |�                �                     � adm-edit-attribute-list ��                �                     � adm-enable   0                �                     �
 adm-exit    <l                �                      adm-hide    x�                �                      adm-initialize  ��                �                      adm-show-errors �$         D	   \                  X2 adm-UIB-mode    4d            
    �                     B adm-view    t�                �                     O dispatch    ��p        �                       � p get-attribute   �p        �    �                   � � get-attribute-list  ,\p           0                  ,� new-state   p�p        �    �                   � �	 notify  ��p        �                      � set-attribute-list  �p       	 �    �                   � � set-position    (Xp       
 �   �                  � �/ ��   �            3 ��P   �        3 ��pT	 ��                   adm-change-page h�                �                     ? delete-page �(p        �    �                   � W init-object 4dp        |   �                  �� init-pages  p�p        �    �                   � �
 select-page ��p        �    �                   � � view-page   �	p        �    �                   � �	 `
g vd	       ]�0
  T]�0
  `          �	    �	�	  ��            w|�	      �by  O ��    e�      O ��    R�      O ��    ��       {�	�	    4 ��l  O{������|    ��                ����                      |	            
              g         Xg �p
       ]6(   �          �
    �
�
  ��            ���
      tcy  O ��    e�      O ��    R�      O ��    ��    �
��}    O�������    ��                ����                      |
                           g            �d�    4 ���        �          ��             ��       ���   �l�@               �@        �      @               � ߱  $ �����        g �       ]n�  }            �    hX  ��            �  x      H��  O ��    e�      O ��    R�      O ��    ��      /��   �D          3 ��,   �        3 ��L  ��                ����                                   �              g         H/ �,   4            3 ��`   @        3 ��x   �T�    4 ���        �          ��             ��       ��   �\        �    ��  ��            ��       D�   ��  O �  ��      O �  ��    0/ �               3 ���   (        3 ���`$ �D���        �               � ߱     �lt    4 ���  k ��        }  �n    �   adm-create-objects  $	�         �                      adm-row-available   ��         L   l                  hP disable_UI  �                �                     b
 enable_UI   (X                �                     m	 local-exit  d�                 �                     w
 send-records    ��            !    h                     � state-changed   �p        � "   �                   � �   � �   ����������� �NO-LOCK         �    }    ��        �     }    �G� s  %              � w  %       	 %        %        %       	 %        %       	 %               %               %               %              %              %              %               %              
�    
"   
OT �     x  � �  � �            �     }    �G� s  � 
"   
OT
"   
�  �      �  �%              
"   
3�%     get-attribute   
"    
   %      TYPE        �  � @
 %      adm/objects/broker.p p�
"    
   %     set-broker-owner QT
"    
   
�    �     }    �G �    �     �     �     �     �     h L    X     H     8     (              � K � S � ` � h � m � m � m ((       
"   
��
%   
           � o       
"   
V�� m � p � m � m � � (T  �   ,       �     }    �G� o      �     }    �G%              � �  T 4    D      4   � � T   %              �     }    �G� � � � T   %              �     }    �G� � %     broker-apply-entry 
"    
   
�    %     broker-destroy  
"    
   
�    %     dispatch p�%     disable-fields %     set-attribute-list % 
    ENABLED=no %               %      adm/support/contnrd.w �
�    %               % 	    enable_UI �
�    %     set-attribute-list %     ENABLED=yes %               %      notify  %      exit    %               %     broker-hide 
"    
   
�    %     broker-initialize T
"    
   
�        %              %                   "      %                  "      �     }    ��    }    �"      %               %     broker-UIB-mode 
"    
   
�    %     broker-view 
"    
   
�    %     broker-dispatch 
"    
   
�    "          �  � f	 % 	    ADM-ERROR T%      broker-get-attribute p�
"    
   
�    "      �  %$     broker-get-attribute-list �
"    
   
�    o%   o           "     %               %     broker-new-state QT
"    
   
�    "      %               %     broker-notify   
"    
   
�    "          �  � f	 % 	    ADM-ERROR T%               %$     broker-set-attribute-list �
"    
   
�    " 	     %               � 
"   
QT
"   
2�    �      x
   � �     " 
  2�%               
"   
   (        �     }    ��      �
  �%                  " 
  2�%               
"   
   (        �     }    ��      D  �%              
"   
2�    �      �   � 
 
"   
�  
�      �  �     " 
  2�%               
"   
3�
" 
  
QTD    (        �     }    ��        �%              �            " 
  2�%               
"   
3�
" 
  
QTD    (        �     }    ��      �  �%              �      �  ( (       " 
     %                   " 
     %              %              ( (       " 
     %                   " 
     %              %              
"   
�Z�      �  " 
   �
"   
�Z�        " 
   %               %     set-attribute-list      � !      
"   
p�%               %     broker-change-page 
"    
   
�    %     broker-delete-page 
"    
   
�    "      %     broker-init-object 
"    
   
�    "      
"   
   "      
"   
   %               %     broker-init-pages T
"    
   
�    "      %     broker-select-page 
"    
   
�    "      %     broker-view-page PT
"    
   
�    "      
"   
OT
"   
OT�     }    �%               
"   
OT%      CLOSE   %               � 
"   
PT
"   
�Z
"   
�Z�      �  %%              
�     }    �
"   
  %     dispatch p�
�    %      destroy %     dispatch p�%     create-objects  �     }    �%     dispatch p�% 
    initialize �    }    ��     �     }    �%     get-attribute   
�    %     Current-Page p� �  "      %               %     init-object 
�    %      adm/objects/folder.w 3�
�         �G          � ) � : � W) 
"   
  %     set-position    
"   
   %            %            %     set-size p�
"   
   %         %        %     add-link p�
"    
   
"   
   %      Page    
�    %              %     init-object 
�    %     PLN/V-PERS1.W �
�         �G% 	    Layout =  1.
"   
   %     set-position    
"   
   %         %       	  %     init-object 
�    %     PLN/Q-PERS.W p�
�         �G%               
"  
 
   %     set-position    
"  
 
   %       	 %       	  %     init-object 
�    %      adm/objects/p-navico.r 
�         �G%| q l   Edge-Pixels = 2,                     SmartPanelType = NAV-ICON,                     Right-to-Left = First-On-Left   
"   
   %     set-position    
"   
   %        %         %     set-size p�
"   
   %         %         %     init-object 
�    %$     adm-vm/objects/p-updv07.w �
�         �G%t j d   Edge-Pixels = 2,                     SmartPanelType = Update,                     AddFunction = One-Record 
"  	 
   %     set-position    
"  	 
   %        %           %     set-size p�
"  	 
   %         %       	 %     add-link p�
"    
   
"  	 
   %      TableIO 
"   
   %     add-link p�
"    
   
"  
 
   %      Record  
"   
   %     add-link p�
"    
   
"   
   % 
    Navigation 
"  
 
   %              %     init-object 
�    %     PLN/B-TITU.W p�
�         �G% 	    Layout =  .W
"   
   %     set-position    
"   
   %         %        %     init-object 
�    %      adm/objects/p-updsav.r 
�         �G%t j d   Edge-Pixels = 2,                     SmartPanelType = Update,                     AddFunction = One-Record 
"   
   %     set-position    
"   
   %         %       	 %     set-size p�
"   
   %       	   %         %     add-link p�
"    
   
"   
   %      TableIO 
"   
   %              %     init-object 
�    %     PLN/B-PROF.W p�
�         �G% 	    Layout =  .W
"   
   %     set-position    
"   
   %          %        %     init-object 
�    %      adm/objects/p-updsav.r 
�         �G%t j d   Edge-Pixels = 2,                     SmartPanelType = Update,                     AddFunction = One-Record 
"   
   %     set-position    
"   
   %          %           %     set-size p�
"   
   %         %         %     add-link p�
"    
   
"   
   %      TableIO 
"   
       "   2�%               %     select-page 
�    %              %     check-modified  
�    %      check   "      %               %     get-attribute   %     FIELDS-ENABLED (       �  � � %              %               %     get-link-handle 
"    
   
�    %     RECORD-SOURCE �"          "   2�� o  %               
�( T    %              "   �Z    �     "      %              � � �     }    �A� �( %      
       � � 
"   
   �      �   �A� 
 %     get-attribute   %     Key-Name PT�      "  
 2�%              %     send-key p�
"   
   "  
    "      %     set-attribute-list �  ��� $ "   -li� 
"   
PT%     dispatch p�
�    %     display-fields %      notify  
�    %     row-available �         �     }    �G� s  � 
"   
OT
"   
   �     }    �
�    
"   
   
"   
   %      CLOSE   %                         h     H 8   ��            ��X       �Tv  O ��    e�      O ��    R�      O ��    ��      /	��    �           3 ���   �         3 ��  ��                ����                          h     H 8   ��            ��X       ,\�  O ��    e�      O ��    R�      O ��    ��      /	��    � 8          3 ��   �         3 ��D  ��                ����                          h     H 8   ��            X       �\�  O ��    e�      O ��    R�      O ��    ��    | i    �         � / �    �             3 ��L   �         3 ��d� / �    �             3 ���   �         3 ���  O ��  ��    ��                  ��    l           ����                          h     H 8   ��            ,X       d]�  O ��    e�      O ��    R�      O ��    ��    � / &�    �             3 ���   �         3 ���  O +��  ���    ��                ����                          h     H 8   ��            6JX       �J�  O ��    e�      O ��    R�      O ��    ��    � /	C�      $          3 ��� / G�    �             3 ��,   �         3 ��L  O I��  ��d    ��                  ��    l           ����                          h     H 8   ��            TbX       dK�  O ��    e�      O ��    R�      O ��    ��    � / _�    �             3 ��x   �         3 ���  O a��  ��    ��                ����                          h     H 8   ��            luX       L�  O ��    e�      O ��    R�      O ��    ��      /	s�    � �          3 ���   �         3 ���  ��                ����                          h     H 8   ��            �X       �x  O ��    e�      O ��    R�      O ��    ��      /	��    �            3 ���   �         3 ��  ��                ����                          �     H 8   ��            ��X      $x  O ��    e�      O ��    R�      O ��    ��      �   �           ��      0       ��       �G�4 �h   $ �� ���                       � ߱  $ �� ���        D               � ߱    4 ��l  	 �,                      3 ���  O ���  ��    > �� T              ��                ����                          h     H 8   ��            ��X        H�  O ��    e�      O ��    R�      O ��    ��      /	��    � �          3 ���   �         3 ���  ��                ����                          h     H 8   ��            ��X       �J�  O ��    e�      O ��    R�      O ��    ��      /	��    �           3 ���   �         3 ��  ��                ����                          |     H 8   ��            ��X       $K�  O ��    e�      O ��    R�      O ��    ��    X        h    � /	��    � 8          3 ���  �         3 ��D   �         3 ��L   �� �     4 ��X  O ���  ��l    > �� �               ��                ����                          |     H 8   ��            ��X       8q�  O ��    e�      O ��    R�      O ��    ��    y        h    � /	��    � �          3 ����  �         3 ���   �         3 ���  O ���  ���    > �� �               ��                ����                          |     H 8   ��            	 X       @��  O ��    e�      O ��    R�      O ��    ��    �        h    /	�    � �          3 ����  �         3 �� 	�  �         3 ��	   � �       3 ��	  $ � ���                          � ߱    O ��  ��(	    > �� (              ��                ����                          |     H 8   ��            *8X       x=�  O ��    e�      O ��    R�      O ��    ��    �        h    � /	5�    � \	          3 ��<	�  �         3 ��h	   �         3 ��p	  O 7��  ��|	    > �� �               ��                ����                          |     H 8   ��            BQX       ���  O ��    e�      O ��    R�      O ��    ��    �        h    � /	L�    � �	          3 ���	�  �         3 ���	   �         3 ���	�  M� �     4 ���	  O N��  ���	    O P��  ���	    > ��               ��                ����                          |     H 8   ��            [lX       �  O ��    e�      O ��    R�      O ��    ��    � 	       h    � /	h�    � 4
          3 ��
�  �         3 ��@
   �         3 ��H
  O k��  ��T
    > ��	 �          	     ��                ����                          �     H 8   ��            v�X      T{  O ��    e�      O ��    R�      O ��    ��    � 
  �    h    � 
       |    � �� �     4 ��h
        �           ��             ��       ���   �� � �� 0�  4 ���
        8          ��             ��       "�   �� | �DL    4 ���
  $ �`���        �
    
           � ߱     ���    4 ��  $ �����        P    
           � ߱     ��    4 ���                  ��             ��       �"�   ��D$ �(���        �   
 
           � ߱  � �PX    4 ���  $ �l���            
           � ߱     ���    4 ��x  $ �����        �    
           � ߱   ���    4 ��  $ �����        l    
           � ߱  � �$    4 ���  $ �8���        �    
           � ߱  @        �     0@                � ߱    $ �T���          O ���  ��<    > ��
 �        �
     ��                ����                          h     H 8   ��            ��X       �#�  O ��    e�      O ��    R�      O ��    ��      /	��    � �          3 ���   �         3 ���  ��                ����                          |     H 8   ��            ��X       P$�  O ��    e�      O ��    R�      O ��    ��    O        h      /��    � �          3 ����  �         3 ��   �         3 ��  > �� �               ��                ����                          �     H 8   ��            �	X       x�  O ��    e�      O ��    R�      O ��    ��    c   �    h    o   
�    | 
   �   �    �    |   
     � 
   l/	�    � <          3 ���  �         3 ��H  �         3 ��P         3 ��\(          3 ��h   4<      3 ��t  $ P���              
            � ߱    O ��  ��    > �� �       � �        ��                ����                          |     H 8   ��            +X       �ʇ  O ��    e�      O ��    R�      O ��    ��    �        h      /	(�    � �          3 ����  �         3 ���   �         3 ���  > �� �               ��                ����                          |     H 8   ��            5DX       H�  O ��    e�      O ��    R�      O ��    ��    O        h      /	A�    � �          3 ����  �         3 ��    �         3 ��  > �� �               ��                ����                          |     H 8   ��            N`X       Â  O ��    e�      O ��    R�      O ��    ��    O        h      /]�    � 4          3 ���  �         3 ��@   �         3 ��H  > �� �               ��                ����                          h     H 8   ��            �JX      ��  O ��    e�      O ��    R�      O ��    ��    � /��    �            3 ��   �         3 ��(D               � ߱  � $ �� ���        �p �L�   E  � X        $          ��             ��       t)�   �� �/�<   D�          3 ��lX P        3 ���l d        3 ���� x        3 ���   ��      3 ���  $ �����              
            � ߱  /	��   �          3 ���� �        3 ��           3 ��0T/	�$   ,\          3 ��D@ 8        3 ��h   L        3 ��|  /�l   t�          3 ���� �        3 ���� �        3 ���   �        3 ���<� �        �          ��             �       *�   ���/�             3 ���, $        3 ��@ 8        3 ��,T L        3 ��<   `h      3 ��T  $ �|���              
            � ߱  �/	��   �|          3 ��`� �        3 ���   �        3 ����/��    �          3 ���         3 ���(          3 ���< 4        3 ���   HP      3 ��  $ �d���              
  
     
     � ߱  �/	��   �8          3 ��� �        3 ��D   �        3 ��Xh/��   ��          3 ��l� �        3 ���         3 ���$         3 ���   08      3 ��@  $ �L���              
            � ߱  �/	��   �h          3 ��L� �        3 ��t   �        3 ����/	 �   ��          3 ���� �        3 ���   �        3 ����/              3 ���, $        3 ��@ 8        3 ��0T L        3 ��@   `h      3 ���  $ |���              
  	     	     � ߱  �/		�   ��          3 ���� �        3 ���   �        3 �� (/	
�    ,          3 ��         3 ��8            3 ��L�/@   Hx          3 ��`\ T        3 ���p h        3 ���   |        3 ����/�   ��          3 ���� �        3 ���� �        3 ���   �        3 ���  /�              3 ��          3 ��$(          3 ��0   4        3 ��H�
| T        �          ��             +       �*�   D$	/�   ��          3 ��h� �        3 ���� �        3 ���� �        3 ���   ��      3 ���  $ 	���              
            � ߱  l	/	<	   D	�          3 ���X	 P	        3 ��    d	        3 ��
/�	   �	@          3 ��(�	 �	        3 ��H�	 �	        3 ��l�	 �	        3 ��|   �	�	      3 ���  $ �	���              
            � ߱  T
/	%$
   ,
          3 �� @
 8
        3 ��(   L
        3 ��<�
/	&l
   t
h          3 ��P�
 �
        3 ��t   �
        3 ���  /)�
   �
�          3 ����
 �
        3 ����
 �
        3 ���   �
        3 ���  8 �        @          ��             -C       t+�   - �/.X   `          3 �� t l        3 �� � �        3 ��<� �        3 ��L   ��      3 ��d  $ .����              
            � ߱  (/	3�    �          3 ��p         3 ���            3 ����/6@   H�          3 ���\ T        3 ���p h        3 ��� |        3 ��   ��      3 ���  $ 6����              
            � ߱  /	=�   ��          3 ���� �        3 ���           3 ���X/	>(   0           3 ���D <        3 ��   P        3 ��   /Ap   xL          3 ��4� �        3 ��X� �        3 ��d   �        3 ��x   G��    4 ���  /H�   ��          3 ���   �        3 ���  > ��               ��                  ��    l           ����                          h     H 8   ��            Q�X      �9�  O ��    e�      O ��    R�      O ��    ��    � /	i�    � �          3 ���   �         3 ���  n� �     4 ��  O n��  ��$  � / p�    �             3 ��8   �         3 ��T$$ q���        p               � ߱  �/	s<   D�          3 ���X P        3 ���l d        3 ���   x�      3 ��    $ s����                          � ߱  � u��    4 ��   O v��  ��(   <    
            � ߱  $ w����        l x    4 ��h   	y4                  <3 ��� D3 ��� L3 ��� T3 ��� \3 ��� d3 ���   3 ��� �/ �   �            3 ���    �        3 ��!�$ �����        ,!     
     
     � ߱  � ��    4 ��0!        $          ��             ��       ��W   ���/�<   Dp!          3 ��X!X P        3 ��|!   dl      3 ���!  $ �����                          � ߱    / ��   �            3 ���!   �        3 ���! ���    4 ���!  /��   �!          3 ���!           3 �� "  /�0   80"          3 ��"   D        3 ��8"  > �� \         �  � ��      ��                ����                          h     H 8   ��            ��X       ��W  O ��    e�      O ��    R�      O ��    ��    �  �t |     4 ��T"  n � �    �"   �� �     4 ���"  � ��"  ��                ����                          h     H 8   ��            ��X       H�W  O ��    e�      O ��    R�      O ��    ��    | 
 ��� x �"      
 ��� �      �"  ��                  ��    l           ����                          h     H 8   ��            ��X       ท  O ��    e�      O ��    R�      O ��    ��    t ��"}    O ���  ���"    ��                ����                                H 8   ��            ��X       T��  O ��    e�      O ��    R�      O ��    ��      ��                ����                                H 8   ��            ��X       仗  O ��    e�      O ��    R�      O ��    ��    �   
�    h 
   �        |      > �� �       �       ��                ����                       d d     �     ��
"�"  � x                                     l                                              (                                      (                                        TXS ok  W-Win h_B-PROF h_B-TITU h_folder h_p-navico h_p-updsav-2 h_p-updsav-3 h_p-updv07 h_Q-PERS h_V-PERS1 F-Main GUI Personal img\calculo Unable to load icon: img\calculo adm-object-hdl adm-query-opened adm-row-avail-state adm-initial-lock adm-new-record adm-updating-record adm-check-modified-all adm-broker-hdl TYPE ADM-Broker ADM1.1` SmartWindow` WINDOW` NO ` `  Layout,Hide-on-Init` ``````````` ^^ ^ ADM-APPLY-ENTRY ADM-DESTROY disable-fields ENABLED=no ADM-DISABLE ADM-EDIT-ATTRIBUTE-LIST ENABLED=yes ADM-ENABLE exit ADM-EXIT ADM-HIDE ADM-INITIALIZE cntr ADM-SHOW-ERRORS ADM-UIB-MODE ADM-VIEW p-method-name ADM-ERROR DISPATCH p-attr-name GET-ATTRIBUTE p-attr-list GET-ATTRIBUTE-LIST p-state NEW-STATE p-method NOTIFY SET-ATTRIBUTE-LIST p-row p-col parent-hdl WINDOW DIALOG-BOX SET-POSITION CURRENT-PAGE=0,ADM-OBJECT-HANDLE= ADM-CHANGE-PAGE p-page# DELETE-PAGE p-proc-name p-parent-hdl p-proc-hdl INIT-OBJECT p-page-list INIT-PAGES SELECT-PAGE VIEW-PAGE CLOSE OK-WAIT-STATE destroy create-objects initialize adm-current-page Current-Page adm/objects/folder.w FOLDER-LABELS =  Personal|Titulos|Profesiones ,                     FOLDER-TAB-TYPE = 2 Page PLN/V-PERS1.W Layout =  PLN/Q-PERS.W adm/objects/p-navico.r Edge-Pixels = 2,                     SmartPanelType = NAV-ICON,                     Right-to-Left = First-On-Left adm-vm/objects/p-updv07.w Edge-Pixels = 2,                     SmartPanelType = Update,                     AddFunction = One-Record TableIO Record Navigation PLN/B-TITU.W adm/objects/p-updsav.r PLN/B-PROF.W ADM-CREATE-OBJECTS tbl-list rowid-list row-avail-cntr row-avail-rowid row-avail-enabled link-handle record-source-hdl different-row key-name key-value check FIELDS-ENABLED YES RECORD-SOURCE row-available in  encountered more than one RECORD-SOURCE. The first -   - will be used. Key-Name Key-Value="&1" display-fields row-available ADM-ROW-AVAILABLE DISABLE_UI ENABLE_UI LOCAL-EXIT SEND-RECORDS p-issuer-hdl STATE-CHANGED �	 �        �   8           (       adm-apply-entry �� `           T       adm-destroy ��< �           |       adm-disable   d �           �       adm-edit-attribute-list &+,  � �           �       adm-enable  CGIJ�                 adm-exit    _ab  � H          <      adm-hide    su$t          d      adm-initialize  ��      �     cntr    L� 	   x    �      adm-show-errors �����  �� 
         �      adm-UIB-mode    ���                adm-view    ��        (  p-method-name   �\        P      dispatch    ���          t  p-attr-name 8�      d  �      get-attribute   ���          �  p-attr-list ��      �  �      get-attribute-list               p-state �<         0      new-state   578          T  p-method    �      D  x      notify  LMNPQ    	      �  p-attr-list `�      �  �      set-attribute-list  hkl    
    �  
   parent-hdl  
        p-row     
        p-col   �P    ��  @      set-position    �������������������  (�          �      adm-change-page ��        �  p-page# x�      �  �      delete-page ��       �  p-proc-name       
  p-parent-hdl    <     0  p-attr-list         L
  p-proc-hdl  �|      �  p      init-object 	          �  p-page-list X�      �  �      init-pages  (+        �  p-page# �      �  �      select-page AD          p-page# �D        8      view-page   ]` `                  {|H|                  ���  d�                  �        �     adm-current-page    ��,    �    �      adm-create-objects  ���������������� 	
%&)+-.36=>ACEGHJd    X     tbl-list    �    t     rowid-list  �    �     row-avail-cntr  �    �     row-avail-rowid �    �     row-avail-enabled        �     link-handle $      
   record-source-hdl   D   	 4     different-row   `   
 T     key-name          p     key-value   ��    H    �      adm-row-available   inpqsuvwxy���������|�          �      disable_UI  ����� 	          	      enable_UI   ���  �L	           @	      local-exit  ���  (	|	 !         l	      send-records    �  �	      �	
  p-issuer-hdl           �	  p-state T	�	 "     �	  �	      state-changed   �  �	d"   " �	            
     
     ok  (
    
  
   W-Win   D
   8
  
   h_B-PROF    `
   T
  
   h_B-TITU    |
   p
  
   h_folder    �
   �
  
   h_p-navico  �
   �
  
   h_p-updsav-2    �
   �
  
   h_p-updsav-3    �
  	 �
  
   h_p-updv07    
   
   h_Q-PERS    ,      
   h_V-PERS1   L   <  
   adm-object-hdl  p   \     adm-query-opened    �   �     adm-row-avail-state �   �     adm-initial-lock    �   �     adm-new-record  �   �     adm-updating-record $        adm-check-modified-all  D    4  
   adm-broker-hdl       T     OK-WAIT-STATE    ( i j { } ~ � � aopqrsty��v��������������  H�   #\\INF203\ON_LI_CO\src\adm\template\row-end.i   �t�   #\\INF203\ON_LI_CO\src\adm\template\row-head.i  ��#   #\\INF203\ON_LI_CO\src\adm\template\windowmn.i  �$   !\\INF203\ON_LI_CO\src\adm\method\containr.i    Pb�   !\\INF203\ON_LI_CO\src\adm\method\smart.i   ��`   !\\INF203\ON_LI_CO\src\adm\method\attribut.i    �Q�    D:\SIE_CO\ON_LI_CO\APLIC\PLN\W-PERS.W                                                                                                                                                                                                                                                                       &ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE ok AS LOGICAL.
ok = SESSION:SET-WAIT-STATE("").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_B-PROF AS HANDLE NO-UNDO.
DEFINE VARIABLE h_B-TITU AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv07 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_Q-PERS AS HANDLE NO-UNDO.
DEFINE VARIABLE h_V-PERS1 AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 87.14 BY 14.19.

 

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Personal"
         HEIGHT             = 14.19
         WIDTH              = 87.14
         MAX-HEIGHT         = 17.73
         MAX-WIDTH          = 91.43
         VIRTUAL-HEIGHT     = 17.73
         VIRTUAL-WIDTH      = 91.43
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

IF NOT W-Win:LOAD-ICON("img\calculo":U) THEN
    MESSAGE "Unable to load icon: img\calculo"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   Default                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Personal */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Personal */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Personal|Titulos|Profesiones' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 14.04 , 86.86 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'PLN/V-PERS1.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_V-PERS1 ).
       RUN set-position IN h_V-PERS1 ( 2.31 , 2.29 ) NO-ERROR.
       /* Size in UIB:  ( 10.65 , 84.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'PLN/Q-PERS.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_Q-PERS ).
       RUN set-position IN h_Q-PERS ( 13.19 , 2.29 ) NO-ERROR.
       /* Size in UIB:  ( 1.62 , 14.86 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 13.23 , 10.43 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 1.58 , 20.14 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm-vm/objects/p-updv07.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv07 ).
       RUN set-position IN h_p-updv07 ( 13.23 , 31.00 ) NO-ERROR.
       RUN set-size IN h_p-updv07 ( 1.58 , 55.29 ) NO-ERROR.

       /* Links to SmartViewer h_V-PERS1. */
       RUN add-link IN adm-broker-hdl ( h_p-updv07 , 'TableIO':U , h_V-PERS1 ).
       RUN add-link IN adm-broker-hdl ( h_Q-PERS , 'Record':U , h_V-PERS1 ).

       /* Links to SmartQuery h_Q-PERS. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_Q-PERS ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'PLN/B-TITU.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_B-TITU ).
       RUN set-position IN h_B-TITU ( 3.23 , 11.72 ) NO-ERROR.
       /* Size in UIB:  ( 9.08 , 49.00 ) */

       RUN init-object IN