DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR x-polo  AS DEC NO-UNDO.
DEF VAR x-bolsa AS DEC NO-UNDO.
DEF VAR x-horario AS DEC NO-UNDO.
DEF VAR x-cartel AS DEC NO-UNDO.
DEF VAR x-codcli AS CHAR FORMAT 'x(11)'.
DEF VAR s-coddiv AS CHAR INIT '00015'.
DEF VAR s-coddoc AS CHAR INIT 'COT'.
DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF VAR s-nroser AS INT INIT 015.
DEF VAR s-codalm AS CHAR INIT '11'.
DEF VAR s-tpoped AS CHAR INIT 'N'.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'ADMIN'.

DEF BUFFER COTIZACION FOR faccpedi.
DEF BUFFER PEDIDO FOR faccpedi.
DEF BUFFER B-DPEDI FOR facdpedi.

INPUT FROM d:\tmp\regalar.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    ASSIGN
        x-codcli = SUBSTRING(x-linea,1,11)
        x-polo = DECIMAL(SUBSTRING(x-linea,21,10))
        x-bolsa = DECIMAL(SUBSTRING(x-linea,31,10))
        x-horario = DECIMAL(SUBSTRING(x-linea,41,10))
        x-cartel = DECIMAL(SUBSTRING(x-linea,51,10)).

    FIND gn-clie WHERE gn-clie.codcia = 000
        AND gn-clie.codcli = x-codcli
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie THEN DO:
        DISPLAY x-codcli.
        NEXT.
    END.
    CREATE Faccpedi.

     {vta2/icorrelativosecuencial.i &Codigo = s-coddoc &Serie = s-nroser} 

    ASSIGN 
        FacCPedi.CodCia = S-CODCIA
        FacCPedi.CodDiv = S-CODDIV
        FacCPedi.CodDoc = s-coddoc 
        FacCPedi.CodAlm = s-CodAlm    /* Lista de Almacenes Válidos de Venta */
        FacCPedi.FchPed = TODAY 
        FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        FacCPedi.TpoPed = s-TpoPed
        FacCPedi.FlgEst = "C".    /* APROBADO */
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    ASSIGN
        faccpedi.codcli = x-codcli
        faccpedi.nomcli = gn-clie.nomcli
        faccpedi.ruccli = gn-clie.ruc
        faccpedi.dircli = gn-clie.dircli
        faccpedi.fchped = TODAY
        faccpedi.fchven = TODAY
        faccpedi.cmpbnte = 'FAC'
        faccpedi.flgigv = YES
        faccpedi.libre_d01 = 4
        faccpedi.codven = '020'
        faccpedi.fmapgo = '900'     /* OJO */
        faccpedi.codmon = 1
        faccpedi.tpocmb = 1
        faccpedi.usuario = 'ADMIN'
        faccpedi.porigv = 18
        faccpedi.codalm = s-codalm.
    IF x-polo > 0 THEN DO:
        FIND almmmatg WHERE almmmatg.codcia = s-codcia
            AND almmmatg.codmat = '073624'
            NO-LOCK.
        CREATE facdpedi.
        BUFFER-COPY faccpedi
            TO facdpedi
            ASSIGN
            facdpedi.almdes = faccpedi.codalm
            facdpedi.codmat = '073624'
            facdpedi.canped = x-polo
            facdpedi.canate = facdpedi.canped
            facdpedi.factor = 1
            facdpedi.undvta = 'UNI'
            facdpedi.preuni = 5.0740
            facdpedi.implin = facdpedi.canped * facdpedi.preuni.
        IF Almmmatg.AftIgv 
            THEN facdpedi.ImpIgv = facdpedi.ImpLin - ROUND( facdpedi.ImpLin  / ( 1 + (faccpedi.PorIgv / 100) ), 4 ).
    END.
    IF x-bolsa > 0 THEN DO:
        FIND almmmatg WHERE almmmatg.codcia = s-codcia
            AND almmmatg.codmat = '070998'
            NO-LOCK.
        CREATE facdpedi.
        BUFFER-COPY faccpedi
            TO facdpedi
            ASSIGN
            facdpedi.almdes = faccpedi.codalm
            facdpedi.codmat = '070998'
            facdpedi.canped = x-bolsa
            facdpedi.canate = facdpedi.canped
            facdpedi.factor = 1
            facdpedi.undvta = 'UNI'
            facdpedi.preuni = 2.4928
            facdpedi.implin = facdpedi.canped * facdpedi.preuni.
        IF Almmmatg.AftIgv 
            THEN facdpedi.ImpIgv = facdpedi.ImpLin - ROUND( facdpedi.ImpLin  / ( 1 + (faccpedi.PorIgv / 100) ), 4 ).
    END.
    IF x-cartel > 0 THEN DO:
        FIND almmmatg WHERE almmmatg.codcia = s-codcia
            AND almmmatg.codmat = '068829'
            NO-LOCK.
        CREATE facdpedi.
        BUFFER-COPY faccpedi
            TO facdpedi
            ASSIGN
            facdpedi.almdes = faccpedi.codalm
            facdpedi.codmat = '068829'
            facdpedi.canped = x-cartel
            facdpedi.canate = facdpedi.canped
            facdpedi.factor = 1
            facdpedi.undvta = 'UNI'
            facdpedi.preuni = 2.0160
            facdpedi.implin = facdpedi.canped * facdpedi.preuni.
        IF Almmmatg.AftIgv 
            THEN facdpedi.ImpIgv = facdpedi.ImpLin - ROUND( facdpedi.ImpLin  / ( 1 + (faccpedi.PorIgv / 100) ), 4 ).
    END.
    IF x-horario > 0 THEN DO:
        FIND almmmatg WHERE almmmatg.codcia = s-codcia
            AND almmmatg.codmat = '074024'
            NO-LOCK.
        CREATE facdpedi.
        BUFFER-COPY faccpedi
            TO facdpedi
            ASSIGN
            facdpedi.almdes = faccpedi.codalm
            facdpedi.codmat = '074024'
            facdpedi.canped = x-horario / 100   /* EN CTOs */
            facdpedi.canate = facdpedi.canped
            facdpedi.factor = 1
            facdpedi.undvta = 'CTO'
            facdpedi.preuni = 0.9856
            facdpedi.implin = facdpedi.canped * facdpedi.preuni.
        IF Almmmatg.AftIgv 
            THEN facdpedi.ImpIgv = facdpedi.ImpLin - ROUND( facdpedi.ImpLin  / ( 1 + (faccpedi.PorIgv / 100) ), 4 ).
    END.
    {vta2/graba-totales-cotizacion-cred.i}

    FIND COTIZACION WHERE ROWID(COTIZACION) = ROWID(Faccpedi).
    RUN Genera-Pedido.
END.

FOR EACH faccpedi WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddiv = s-coddiv
    AND faccpedi.coddoc = 'O/D'
    AND faccpedi.usuario = s-user-id
    AND faccpedi.fchped = TODAY
    AND faccpedi.flgest = 'P'
    AND faccpedi.flgsit = 'T':
    faccpedi.flgsit = 'C'.
END.

PROCEDURE Genera-Pedido:

DEF VAR s-coddoc AS CHAR INIT 'PED'.
DEF VAR s-nroser AS INT INIT 015.
DEF VAR s-codref AS CHAR INIT 'COT'.

      /* Bloqueamos el correlativo para controlar las actualizaciones multiusuario */
  DEF VAR iLocalCounter AS INTEGER INITIAL 0 NO-UNDO.
  GetLock:
  DO ON STOP UNDO GetLock, RETRY GetLock:
      IF RETRY THEN DO:
          iLocalCounter = iLocalCounter + 1.
          IF iLocalCounter = 5 THEN LEAVE GetLock.
      END.
      FIND FacCorre WHERE FacCorre.CodCia = s-CodCia AND
          FacCorre.CodDoc = s-coddoc AND
          FacCorre.NroSer = s-nroser EXCLUSIVE-LOCK NO-ERROR.
  END. 
  IF iLocalCounter = 5 OR NOT AVAILABLE FacCorre THEN UNDO, RETURN "ADM-ERROR".

  CREATE Faccpedi.

  {vta2/icorrelativosecuencial.i &Codigo = s-coddoc &Serie = s-nroser}

  BUFFER-COPY COTIZACION
      TO Faccpedi
      ASSIGN 
      Faccpedi.CodDoc = s-coddoc 
      Faccpedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
      Faccpedi.CodRef = COTIZACION.coddoc
      Faccpedi.NroRef = COTIZACION.nroped
      Faccpedi.FlgEst = "P"
      FacCPedi.Libre_c01 = s-User-id + '|' + STRING(DATETIME(TODAY, MTIME), '99/99/9999 HH:MM')
      .
  ASSIGN
    FacCorre.Correlativo = FacCorre.Correlativo + 1.
  ASSIGN
      Faccpedi.Usuario = S-USER-ID
      Faccpedi.Hora   = STRING(TIME,"HH:MM").
  /* Division destino */
  FIND Almacen OF Faccpedi NO-LOCK NO-ERROR.
  IF AVAILABLE Almacen THEN FacCPedi.DivDes = Almacen.CodDiv.

  DEF VAR I-NPEDI AS INT.
  I-NPEDI = 0.
  FOR EACH B-DPEDI OF COTIZACION NO-LOCK:
      I-NPEDI = I-NPEDI + 1.
      CREATE Facdpedi.
      BUFFER-COPY B-DPEDI
          TO Facdpedi
          ASSIGN
              Facdpedi.CodCia = Faccpedi.CodCia
              Facdpedi.CodDiv = Faccpedi.CodDiv
              Facdpedi.coddoc = Faccpedi.coddoc
              Facdpedi.NroPed = Faccpedi.NroPed
              Facdpedi.FchPed = Faccpedi.FchPed
              Facdpedi.Hora   = Faccpedi.Hora 
              Facdpedi.FlgEst = Faccpedi.FlgEst
              Facdpedi.NroItm = I-NPEDI
              Facdpedi.CanAte = 0.
  END.
  /* Grabamos Totales */
  {vta2/graba-totales-cotizacion-cred.i}


  RUN vta2/pcreaordendesp ( ROWID(Faccpedi) ).

END PROCEDURE.

