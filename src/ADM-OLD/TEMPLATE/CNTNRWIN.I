
DEFINE SUB-MENU m_Archivo 
       MENU-ITEM m_Salir        LABEL "Salir"         .

DEFINE SUB-MENU m_Herramientas 
       MENU-ITEM m_Calculadora  LABEL "Calculadora"   
       MENU-ITEM m_Calendario   LABEL "Calendario"    
       MENU-ITEM m_Block_de_Notas LABEL "Block de Notas".

DEFINE SUB-MENU m_Ayuda
       MENU-ITEM m_Contenido     LABEL "Contenido"
       MENU-ITEM m_Temas_Ayuda   LABEL "Temas de Ayuda"
       MENU-ITEM m_Mensaje       LABEL "Mensajes de Error"
       RULE
       MENU-ITEM m_Acerca        LABEL "Acerca de ..".


DEFINE MENU MENU-BAR-W-Win MENUBAR
       SUB-MENU  m_Archivo      LABEL "Archivo"       
       SUB-MENU  m_Herramientas LABEL "Herramientas"  
       SUB-MENU m_Ayuda        LABEL "Ayuda"         .

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-W-Win:HANDLE.


 
ON CHOOSE OF MENU-ITEM m_Salir
DO:
   IF "{&PROCEDURE-TYPE}" = "SmartWindow" 
   THEN APPLY "CLOSE":U TO THIS-PROCEDURE.
   ELSE APPLY "CLOSE":U TO CURRENT-WINDOW.
END.

ON CHOOSE OF MENU-ITEM m_Block_de_Notas /* Block de Notas */
DO:
   RUN WinExec (INPUT "NOTEPAD.EXE", INPUT 1).
END.

ON CHOOSE OF MENU-ITEM m_Calculadora /* Calculadora */
DO:
     RUN WinExec (INPUT "CALC.EXE", INPUT 1).  /* 1=normal 2=minimized */  
   
END.

ON CHOOSE OF MENU-ITEM m_Calendario 
DO:
   run EJECUTA("bin/_calenda.r").
END.

ON CHOOSE OF MENU-ITEM m_Mensaje 
DO:
   run EJECUTA("prohelp/_msgs.p").
END.

ON CHOOSE OF MENU-ITEM m_Acerca 
DO:
    run EJECUTA("bin/_acerca.w").
END.



PROCEDURE WinExec EXTERNAL "krnl386.exe": /* Run Windows application */
    DEFINE INPUT PARAMETER prog_name AS CHARACTER. 
    DEFINE INPUT PARAMETER prog_style AS SHORT. 
END PROCEDURE.

PROCEDURE EJECUTA.
    DEFINE INPUT PARAMETER PROGRAMA AS CHARACTER NO-UNDO.
    DEFINE VARIABLE OK AS LOGICAL.
    
    OK = SESSION:SET-WAIT-STATE("GENERAL").
    DO ON STOP UNDO, LEAVE ON ERROR UNDO, LEAVE:
       RUN VALUE( PROGRAMA ).
    END.
    OK = SESSION:SET-WAIT-STATE("").
    
END PROCEDURE.


