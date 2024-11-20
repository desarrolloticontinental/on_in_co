DEF VAR x-codcli AS CHAR FORMAT 'x(11)'.
DEF VAR x-implin AS DEC FORMAT '>>>,>>>,>>9.99'.
DEF VAR x-linea AS CHAR FORMAT 'x(100)'.
DEF VAR s-user-id AS CHAR INIT 'ADMIN'.
DEF VAR s-codcia AS INT INIT 001.

INPUT FROM c:\tmp\LC.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea <> '' THEN DO:
        ASSIGN
            x-codcli = SUBSTRING(x-linea,1,11)
            x-implin = DEC(SUBSTRING(x-linea,12)).
        FIND gn-clie WHERE codcia = 000
            AND codcli = x-codcli
            NO-LOCK.
        /* rastreamos ultima linea de credito */
        FOR LAST gn-cliel OF gn-clie BY fchini BY fchfin:
            IF gn-cliel.fchfin >= TODAY THEN gn-cliel.fchfin = TODAY - 1.
        END.
        /* creamos la nueva linea de credito */
        CREATE Gn-ClieL.
        ASSIGN
            Gn-ClieL.CodCia = gn-clie.codcia
            Gn-ClieL.CodCli = gn-clie.codcli
            gn-cliel.fchaut[1] = TODAY
            gn-cliel.fchini = TODAY
            gn-cliel.fchfin = 10/31/2013
            gn-cliel.monlc  = 1
            gn-cliel.implc  = x-implin
            gn-cliel.usrlc  = s-user-id.
        CREATE LogTabla.
        ASSIGN
          logtabla.codcia = s-codcia
          logtabla.Dia = TODAY
          logtabla.Evento = 'LINEA-CREDITO'
          logtabla.Hora = STRING(TIME, 'HH:MM')
          logtabla.Tabla = 'GN-CLIEL'
          logtabla.Usuario = s-user-id
          logtabla.ValorLlave = STRING(gn-clie.codcli, 'x(11)') + '|' +
                                  STRING(gn-clie.nomcli, 'x(50)') + '|' +
                                  STRING(gn-clieL.MonLC, '9') + '|' +
                                  STRING(gn-clieL.ImpLC, '->>>>>>>>9.99') + '|' +
                                  STRING(gn-clie.CndVta, 'x(4)').
   END.
END.
INPUT CLOSE.

