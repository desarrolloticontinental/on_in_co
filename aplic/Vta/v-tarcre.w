-Dialog
ON LEAVE OF F-PREUNI IN FRAME D-Dialog /* Precio */
DO:
    APPLY "RETURN" TO F-PREUNI.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-PREUNI D-Dialog
ON RETURN OF F-PREUNI IN FRAME D-Dialog /* Precio */
DO:
  
  DEFINE VARIABLE pre1 AS DECIMAL.
  DEFINE VARIABLE pre2 AS DECIMAL.
  
  pre1 = 0.
  pre2 = 0.

  ASSIGN
    F-PREUNI.
    
/***************/

    F-FACTOR = 1.

    /****   Busca el Factor de conversion   ****/
    IF X-UNIVTA <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = X-UNIVTA
                       AND  Almtconv.Codalter = X-UNIBAS
                      NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv THEN DO:
           MESSAGE "Codigo de unidad no exixte" VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
        END.
        F-FACTOR = Almtconv.Equival /* / Almmmatg.FacEqu*/.
    END.
    /*******************************************/

    X-NEWPRE = F-PREUNI.

    /****   Convierte al tipo de moneda ****/.
    IF S-CODMON = 1 THEN
      ASSIGN Pre1 = X-NEWPRE
             Pre2 = ROUND(X-NEWPRE / X-TpoCmb,6).
    ELSE
      ASSIGN Pre2 = X-NEWPRE
             Pre1 = ROUND(X-NEWPRE * X-TpoCmb,6).

    X-NEWPRE = IF X-MonVta = 1 THEN Pre1
                               ELSE Pre2.

    X-NEWDSC = ROUND((1 - (X-NEWPRE / F-FACTOR) / X-VALVTA) * 100, 2).

    X-NEWPRE = F-PREUNI.

/****************/    
    
    
  CASE X-NIV:
    WHEN "1" THEN DO:
        IF (X-NEWDSC - X-DSCTO) > FacCfgGn.DtoMax THEN DO:
            MESSAGE "Ud. no puede excederse del " FacCfgGn.DtoMax
                    VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO F-PREUNI IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
    END.
    WHEN "2" THEN 
        IF (X-NEWDSC - X-DSCTO) > FacCfgGn.DtoDis THEN DO:
            MESSAGE "Ud. no puede excederse del " FacCfgGn.DtoDis
                    VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO F-PREUNI IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
    WHEN "3" THEN
        IF (X-NEWDSC - X-DSCTO) > FacCfgGn.DtoMay THEN DO:
            MESSAGE "Ud. no puede excederse del " FacCfgGn.DtoMay
                    VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO F-PREUNI IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
  END CASE.

/*  X-NEWDSC = X-NEWDSC + F-DSCTO.*/
  IF X-NEWDSC > X-MAXDSC THEN DO:
    MESSAGE "Ud. no puede hacer un descuento" SKIP
            "mayor al " X-MAXDSC "%"
            VIEW-AS ALERT-BOX ERROR.
            X-NEWDSC = X-NEWDSC - X-DSCTO.
            APPLY "ENTRY" TO F-PREUNI IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
  END.
  
  APPLY "END-ERROR":U TO SELF.
  RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-2 FILL-IN-3 F-PREUNI 
      WITH FRAME D-Dialog.
  ENABLE RECT-28 F-PREUNI 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    DISPLAY X-MAXDSC @ FILL-IN-2
            X-DSCTO @ FILL-IN-3.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Parametros D-Dialog 
PROCEDURE Procesa-Parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    Variables a usar:
    output-var-1 como ROWID
    output-var-2 como CHARACTER
    output-var-3 como CHARACTER.
    */

    CASE HANDLE-CAMPO:name:
        WHEN "" THEN .
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recoge-Parametros D-Dialog 
PROCEDURE Recoge-Parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    Variables a usar:
    input-var-1 como CHARACTER
    input-var-2 como CHARACTER
    input-var-3 como CHARACTER.
    */

    CASE HANDLE-CAMPO:name:
        WHEN "" THEN ASSIGN input-var-1 = "".
        /*
            ASSIGN
                input-para-1 = ""
                input-para-2 = ""
                input-para-3 = "".
         */      
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 	��VS̸:    � 4              �                                ��        p)              ��  h%0$      �%  ) �M  �  dN  �  (O  H pP  �  DQ  4 xR  �  LS  �  T  �  �T  �	 XV  � 
 W  �  �W  ( Y  
    \   @   Q     p   T   Z          h   �c 
 �     �                                            m        T     �                                            v                 INTEGRAL                         PROGRESS                         �                �I7:      � ��                �    �
   
                �4     �                                            �      
�      +      7      C     
            � ߱  �$ x����        �/	��   �          3 ���    �        3 ��� �D    4 ��0        L          ��             ��       d�   �l/�d                3 ��D�$ �����        h   
              � ߱    /��   ��          3 ��t   �        3 ���h$ �����        �@        �       � ߱  adm-apply-entry                    �                     � adm-destroy @                �                      adm-disable L|                                    + adm-edit-attribute-list ��                �                     7 adm-enable  �                 �                     [
 adm-exit    <                �                     k adm-hide    Hx                �                     t adm-initialize  ��                �                     } adm-show-errors ��         D	   \                  X� adm-UIB-mode    4            
    �                     � adm-view    Dt                �                     � dispatch    ��p        �                       � � get-attribute   ��p        � 
 �    �                   � 	 notify  |�p        �                      , set-attribute-list  ��p        �    �                   � 3 set-position    �(p       
 select-page |�p        �    �                   � 	 view-page   ��p        �    �                   � 	 ,                � ߱  T	$ �$	���        �o �    t	  L             DX
/ �
                 3 ���  ��   ��                ����                      �	            (
              g                   �
    �
�
  ��            ���
      �S�  O ��    e�      O ��    R�      O ��    ��      � $  ��                ����                      �
            �
              g              "P
          <                � ߱  �$ �d���        �g �       ] �              H    ��  ��                  ���  O ��    e�      O ��    R�      O ��    ��    p@        `     �@        �       � ߱    $ ���          ��                  ��    �           ����                      �            \              g         �g �        ]4T             L
           P   
             � ߱  4$ <����        �/ <L   T            3 ��`h `        3 ��|| t        3 ���   ��      3 ���  $ <����                           � ߱  � <�    4 ���        D          ��             <<       ��   <��@        �     @        �       � ߱  X$ <���          p <,h�<  �� <D  L                � ߱    $ <t���          � T\                � ߱    $ <����          O <��  ��d     <��  4 ����@        �       � ߱    $ < ���        �@        �     @             `@        P     �@        |     �@        �       � ߱    $ <0���                �          ��             <<       t��   <�   <�4    4 ���<@        ,     l@        \       � ߱    $ <���          ��                  ��    �           ����                                  H              g         adm-busca   ��                �                     E	 adm-imprime ��                 �                     X _busca-lookup   �,p    (    !   @                  <�
        ��   �T	D                  L3 ���  3 ���`�   O	������  x �   
 enable_UI   8#h#            )    �                     }	 local-initialize    t#�#            *                         � Procesa-Parametros  �#�#            +    �                     � Recoge-Parametros   �#,$            ,    �                     � send-records    @$p$            -    h                     � state-changed   �$�$p        � .   �                   � �
�         �G% 
"    
   %      TYPE        �  � �
 %      adm/objects/broker.p }
"    
   %     set-broker-owner QT
"    
   
�    �     }    �G �    �     �     �     �     �     h L    X     H     8     (              � � � � � � � � � � � � � � ((       
"   
��
%   
           � �       
"   
Sv� � � � � � � � � � (T  �   ,       �     }    �G� �      �     }    �G%              � �  T 4    D      4   � � T   %              �     }    �G� � � � T   %              �     }    �G� � %     broker-apply-entry 
"    
   
�    %     broker-destroy  
"    
   
�    %     dispatch }%     disable-fields %     set-attribute-list % 
    ENABLED=no %               %      adm/support/contnrd.w }
�    %               % 	    enable_UI }
�    %     set-attribute-list %     ENABLED=yes %               %      notify  %      exit    %               %     broker-hide 
"    
   
�    %     broker-initialize T
"    
   
�        %              %                   "      %                  "      �     }    ��    }    �"      %               %     broker-UIB-mode 
"    
   
�    %     broker-view 
"    
   
�    %     broker-dispatch 
"    
   
�    "          �  � �	 % 	    ADM-ERROR T%      broker-get-attribute }
"    
   
�    "      �  %$     broker-get-attribute-list }
"    
   
�    o%   o           " 	    %               %     broker-new-state QT
"    
   
�    " 
     %               % 
"    
   
�    "          �  � �	 % 	    ADM-ERROR T%               %$     broker-set-attribute-list }
"    
   
�    "      %               � 
"   
QT
"   
��    �      P	   � ]     " 
"   
   (        �     }    ��      �	  �%                  " 
"   
   (        �     }    
  
"   
��    �      l
   � d
 
"   
�  
�      �
  �     " 
"   
��
" 
QTD    (        �     }    ��      �
  �%              �      �
      " 
"   
��
" 
QTD    (        �     }    
"   
���      �  " 
"   
���      �  " 
"   
}%               %     broker-change-page 
"    
   
�    %     broker-delete-page 
"    
   
�    "      %     broker-init-object 
"    
   
�    "      
"   
   "      
"