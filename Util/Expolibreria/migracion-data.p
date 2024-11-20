/* RUTINAS DE MIGRACION DE INFORMACION PARA EXPOLIBRERIA */

DISPLAY 'clientes'.
PAUSE 0.
OUTPUT TO c:\tmp\gn-clie.d.
FOR EACH gn-clie NO-LOCK WHERE codcia = 000:
    EXPORT gn-clie.
END.
OUTPUT CLOSE.

OUTPUT TO c:\tmp\gn-clieb.d.
FOR EACH gn-clieb NO-LOCK:
    EXPORT gn-clieb.
END.
OUTPUT CLOSE.

OUTPUT TO c:\tmp\gn-cliel.d.
FOR EACH gn-cliel NO-LOCK:
    EXPORT gn-cliel.
END.
OUTPUT CLOSE.

OUTPUT TO c:\tmp\gn-cliem.d.
FOR EACH gn-cliem NO-LOCK:
    EXPORT gn-cliem.
END.
OUTPUT CLOSE.

DISPLAY 'tarjetas'.
PAUSE 0.
OUTPUT TO c:\tmp\gn-card.d.
FOR EACH gn-card NO-LOCK:
    EXPORT gn-card.
END.
OUTPUT CLOSE.

DISPLAY 'catalogo'.
PAUSE 0.
OUTPUT TO c:\tmp\almmmatg.d.
FOR EACH almmmatg NO-LOCK WHERE almmmatg.codcia = 001:
    EXPORT almmmatg.
END.
OUTPUT CLOSE.

DISPLAY 'vendedores'.
PAUSE 0.
OUTPUT TO c:\tmp\gn-ven.d.
FOR EACH gn-ven NO-LOCK:
    EXPORT gn-ven.
END.
OUTPUT CLOSE.

DISPLAY 'cotizaciones'.
PAUSE 0.
OUTPUT TO c:\tmp\faccpedi.d.
FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = 001
    AND faccpedi.coddiv = '00015'
    AND faccpedi.coddoc = 'cot'
    AND faccpedi.fchped >= 10/01/2009:
    EXPORT faccpedi.
END.
OUTPUT CLOSE.

OUTPUT TO c:\tmp\facdpedi.d.
FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = 001
    AND faccpedi.coddiv = '00015'
    AND faccpedi.coddoc = 'cot'
    AND faccpedi.fchped >= 10/01/2009,
    EACH facdpedi OF faccpedi NO-LOCK:
    EXPORT facdpedi.
END.
OUTPUT CLOSE.

DISPLAY 'terminales'.
PAUSE 0.
OUTPUT TO c:\tmp\exptermi.d.
FOR EACH exptermi NO-LOCK:
    EXPORT exptermi.
END.
OUTPUT CLOSE.
