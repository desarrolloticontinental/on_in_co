
FOR EACH PF-G004 WHERE
    INDEX(Seguridad,"Asistente Producción") > 0:
    /*
    Seguridad = REPLACE(Seguridad,"Asistente Produccion","Asistente Producción").
    */
    DISPLAY
        Aplic-Id
        CodCia
        Seguridad
        User-Id
