OUTPUT TO d:\tmp\menus.txt.
PUT UNFORMATTED
    "APLICACION|OPCION DEL MENU|TIPO|GRUPOS DE ACCESO"
    SKIP.
FOR EACH pf-g002 NO-LOCK:
    PUT UNFORMATTED 
        aplic-id '|'
        etiqueta '|'
        tipo '|'
        seguridad-grupos '|'
        SKIP.
END.
OUTPUT CLOSE.


