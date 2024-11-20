  DEF TEMP-TABLE T-CAB
    FIELD CodCia AS integer 
    FIELD CodDoc AS character FORMAT "x(3)"
    FIELD NroPed AS character FORMAT "X(9)"
    FIELD FchPed AS date FORMAT "99/99/9999"
    FIELD CodCli AS character FORMAT "x(11)"
    FIELD NomCli AS character FORMAT "x(50)"
    FIELD DirCli AS character FORMAT "x(60)"
    FIELD RucCli AS character FORMAT "x(11)"
    FIELD Hora AS character FORMAT "X(5)"
    FIELD FlgEst AS character FORMAT "X(1)" 
    FIELD CodAlm AS character FORMAT "x(3)"
    FIELD CodMon AS integer   FORMAT "9"
    FIELD TpoCmb AS decimal FORMAT "Z,ZZ9.9999"
    FIELD usuario AS character FORMAT "x(10)"
    FIELD Glosa AS character FORMAT "X(50)"
    FIELD ImpBrt AS decimal FORMAT "->>,>>>,>>9.99"
    FIELD ImpExo AS decimal FORMAT "->>,>>>,>>9.99"
    FIELD PorIgv AS decimal FORMAT "->>9.99"
    FIELD ImpDto AS decimal FORMAT "->>,>>>,>>9.99"
    FIELD ImpTot AS decimal FORMAT "->>,>>>,>>9.99"
    FIELD ImpIgv AS decimal FORMAT "->>,>>>,>>9.99"
    FIELD CodVen AS character FORMAT "x(10)"
    FIELD ImpIsc AS decimal FORMAT "->>,>>>,>>9.99"
    FIELD ImpVta AS decimal FORMAT "->>,>>>,>>9.99"
    FIELD FlgSit AS character FORMAT "X"
    FIELD FmaPgo AS character FORMAT "X(8)"
    FIELD fchven AS date FORMAT "99/99/99"
    FIELD ordcmp AS character FORMAT "X(12)"
    FIELD CodDiv AS character FORMAT "x(5)"
    FIELD TipVta AS character FORMAT "X(1)"
    FIELD PorDto AS decimal FORMAT ">>9.99"
    FIELD TpoPed AS character FORMAT "X(1)"
    FIELD UsrDscto AS character FORMAT "X(10)"
    FIELD LugEnt AS character FORMAT "x(60)"
    FIELD CodTrans AS character FORMAT "X(8)"
    FIELD NroRef AS character FORMAT "X(9)"
    FIELD Cmpbnte AS character FORMAT "X(3)"
    FIELD Observa AS character FORMAT "X(50)"
    FIELD NCmpbnte AS character FORMAT "X(9)"
    FIELD Atencion AS character FORMAT "X(30)"
    FIELD FaxCli AS character FORMAT "x(13)"
    FIELD LugEnt2 AS character FORMAT "x(60)"
    FIELD ImpFle AS decimal FORMAT "->>,>>>,>>9.99"
    FIELD FlgIgv AS logical FORMAT "Si/no"
    FIELD TpoLic AS logical FORMAT "si/no"
    FIELD AcuBon AS decimal FORMAT "->>>,>>>,>>9.99"
    FIELD NroCard AS character FORMAT "x(8)"
    FIELD TipBon AS integer FORMAT "99"
    FIELD importe AS decimal FORMAT ">,>>>,>>9.99"
    FIELD porcent AS decimal FORMAT "->>9.99"
    FIELD FchEnt AS date FORMAT "99/99/9999"
    FIELD CodPos AS character FORMAT "x(3)"
    FIELD UsrAprobacion AS character FORMAT "x(10)"
    FIELD FchAprobacion AS date FORMAT "99/99/99"
    INDEX llave03 AS UNIQUE PRIMARY CodCia CodDiv CodDoc NroPed. 

  DEF TEMP-TABLE T-DET LIKE FacDPedi.

 
  DEF TEMP-TABLE DCMP LIKE integral.LG-DOCMP.
  
  DEF VAR s-CodCia AS INT INIT 001 NO-UNDO.
  DEF VAR s-CodMon AS INT INIT 1 NO-UNDO.
  DEF VAR s-TpoCmb AS DEC NO-UNDO.
  DEF VAR x-StkMin AS DEC NO-UNDO.
  DEF VAR x-StkAct AS DEC NO-UNDO.
  DEF VAR x-Dias   AS INT INIT 1 NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR f-Factor AS DEC NO-UNDO.
  
  FOR EACH DCMP:
    DELETE DCMP.
  END.
  
  /* BARREMOS LAS ORDENES DE COMPRA */
  INPUT FROM c:\tmp\faccpedi.d.
  REPEAT:
    CREATE T-CAB.
    IMPORT T-CAB.
  END.
  INPUT CLOSE.











