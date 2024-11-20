DEF BUFFER MATG FOR almmmatg.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR x-NroCor AS INT.
DEF TEMP-TABLE t-matg LIKE almmmatg.
DEF VAR x-ordmat AS INT.
DEF VAR C-ALM AS CHAR.

DEF VAR x-linea AS CHAR FORMAT 'x(200)'.
DEF VAR x-cuenta AS INT INIT 1.

INPUT FROM c:\tmp\surquillo.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea <> '' THEN DO:
        CREATE t-matg.
        ASSIGN
            t-matg.codcia = s-codcia
            t-matg.codmat = STRING(x-cuenta, '999999')
            t-matg.desmat = SUBSTRING(x-linea,1,65)
            t-matg.CHR__02 = SUBSTRING(x-linea,66,1)
            t-matg.codfam = SUBSTRING(x-linea,67,3)
            t-matg.subfam = SUBSTRING(x-linea,70,3)
            t-matg.tpopro = (IF SUBSTRING(x-linea,73,1) = 'S' THEN 'Importado' ELSE 'Nacional')
            t-matg.codpr1 = SUBSTRING(x-linea,74,8)
            t-matg.codmar = SUBSTRING(x-linea,82,4)
            t-matg.undbas = "UNI"
            t-matg.undcmp = "UNI"
            t-matg.undstk = "UNI"
            t-matg.CHR__01 = "UNI"
            t-matg.unda = "UNI"
            t-matg.aftigv = YES
            t-matg.aftisc = NO
            t-matg.tpoart = 'A'
            t-matg.usuario = 'ADMIN'.
        FIND almtabla WHERE almtabla.Tabla = "MK" AND
            almtabla.Codigo = t-matg.codmar NO-LOCK NO-ERROR.
        IF AVAILABLE almtabla THEN t-matg.desmar = almtabla.Nombre.
        x-cuenta = x-cuenta + 1.
    END.
END.
INPUT CLOSE.

FOR EACH t-matg:
    /* Capturamos Correlativo */
    FIND LAST MATG WHERE MATG.CodCia = S-CODCIA NO-LOCK NO-ERROR.
    IF AVAILABLE MATG THEN x-NroCor = INTEGER(MATG.codmat) + 1.
    ELSE x-NroCor = 1.
    FIND LAST MATG WHERE MATG.Codcia = S-CODCIA 
                    AND  MATG.CodFam = t-matg.Codfam
                   USE-INDEX Matg08 NO-LOCK NO-ERROR.
    IF AVAILABLE MATG 
    THEN x-ordmat = MATG.Orden + 3.
    ELSE x-ordmat = 1.
    CREATE Almmmatg.
    BUFFER-COPY t-matg TO Almmmatg
        ASSIGN
            Almmmatg.codmat = STRING(x-NroCor,"999999")
            Almmmatg.orden  = x-ordmat
            Almmmatg.ordlis = x-ordmat
            Almmmatg.tpoart = 'A'     /* Activo */
            Almmmatg.FchIng = TODAY
            Almmmatg.FchAct = TODAY.
    /* Actualizamos la lista de Almacenes */ 
    /*ALM = TRIM(Almmmatg.almacenes).*/
    C-ALM = ''.
    FOR EACH Almacen NO-LOCK WHERE Almacen.CodCia = Almmmatg.codcia AND Almacen.TdoArt:
        IF C-ALM = "" THEN C-ALM = TRIM(Almacen.CodAlm).
        IF LOOKUP(TRIM(Almacen.CodAlm),C-ALM) = 0 THEN C-ALM = C-ALM + "," + TRIM(Almacen.CodAlm).
    END.
    ASSIGN 
        Almmmatg.almacenes = C-ALM.
    RUN ACTUALIZA-MAT-x-ALM.  
    RELEASE Almmmatg.
END.



PROCEDURE ACTUALIZA-MAT-x-ALM:

     FOR EACH Almacen NO-LOCK WHERE Almacen.CodCia = Almmmatg.codcia AND
            Almacen.TdoArt:
         FIND Almmmate WHERE Almmmate.CodCia = Almmmatg.codcia AND 
              Almmmate.CodAlm = Almacen.CodAlm AND 
              Almmmate.CodMat = Almmmatg.CodMat NO-ERROR.
         IF NOT AVAILABLE Almmmate THEN DO:
            CREATE Almmmate.
            ASSIGN Almmmate.CodCia = Almmmatg.codcia
                   Almmmate.CodAlm = Almacen.CodAlm
                   Almmmate.CodMat = Almmmatg.CodMat.
         END.
         ASSIGN Almmmate.DesMat = Almmmatg.DesMat
                Almmmate.FacEqu = Almmmatg.FacEqu
                Almmmate.UndVta = Almmmatg.UndStk
                Almmmate.CodMar = Almmmatg.CodMar.
         FIND FIRST almautmv WHERE 
              almautmv.CodCia = Almmmatg.codcia AND
              almautmv.CodFam = Almmmatg.codfam AND
              almautmv.CodMar = Almmmatg.codMar AND
              almautmv.Almsol = Almmmate.CodAlm NO-LOCK NO-ERROR.
         IF AVAILABLE almautmv THEN 
            ASSIGN Almmmate.AlmDes = almautmv.Almdes
                   Almmmate.CodUbi = almautmv.CodUbi.
         RELEASE Almmmate.
     END.

END PROCEDURE.

