
FOR EACH PF-G004 WHERE
    INDEX(Seguridad,"Asistente Producci�n") > 0:
    /*
    Seguridad = REPLACE(Seguridad,"Asistente Produccion","Asistente Producci�n").
    */
    DISPLAY
        Aplic-Id
        CodCia
        Seguridad
        User-Id
