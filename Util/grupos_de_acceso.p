FOR EACH PF-G002 WHERE
    /*
    Aplic-Id = "Alm" and
    */
    INDEX(Seguridad-Grupos,"Asistente Producci�n") > 0 :
    /*
    Seguridad-Grupos = REPLACE(Seguridad-Grupos,",Asistente Producci�n",",").
    */
    DISPLAY
        Aplic-Id
        Acceso-directo
        CodMnu
        Etiqueta
        Icon
        Persistente
        Programa
        Seguridad-Atributos
        Seguridad-Grupos
        Tecla-Aceleradora
        Tipo
        WITH STREAM-IO.



        
