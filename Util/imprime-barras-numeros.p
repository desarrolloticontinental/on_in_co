DEF STREAM REPORTE.
  DEF VAR x-cuenta AS INT FORMAT '>>>9'.
  DEF VAR x-Codigo AS CHAR FORMAT '9999'.
  DEF VAR rpta AS LOG.
  DEF VAR x-Copias AS INT INIT 1.
  
  SYSTEM-DIALOG PRINTER-SETUP UPDATE rpta.
  IF rpta = NO THEN RETURN.
  OUTPUT STREAM REPORTE TO PRINTER.

  DO x-Cuenta = 1 TO 50 BY 1:
      x-Codigo = STRING(x-Cuenta, '9999').
      PUT STREAM REPORTE '^XA^LH000,012'               SKIP.   /* Inicia formato */
      PUT STREAM REPORTE '^FO170,00'                     SKIP.   /* Coordenadas de origen campo1 */
      PUT STREAM REPORTE '^A0R,25,15'                    SKIP.
      PUT STREAM REPORTE '^FD'.
      PUT STREAM REPORTE x-Cuenta                        SKIP.
      PUT STREAM REPORTE '^FS'                           SKIP.   /* Fin de Campo1 */
      PUT STREAM REPORTE '^FO80,30'                      SKIP.   /* Coordenadas de origen barras */

      PUT STREAM REPORTE '^BY2^BCR,80,Y,N,N'         SKIP.   /* Codigo 128 */
      PUT STREAM REPORTE '^FD>;'.
      PUT STREAM REPORTE x-Codigo SKIP.
      PUT STREAM REPORTE '^FS'                       SKIP.   /* Fin de Campo2 */

      PUT STREAM REPORTE '^LH210,012'                  SKIP.   /* Inicia formato */
      PUT STREAM REPORTE '^FO170,00'                     SKIP.   /* Coordenadas de origen campo1 */
      PUT STREAM REPORTE '^A0R,25,15'                    SKIP.
      PUT STREAM REPORTE '^FD'.
      PUT STREAM REPORTE x-Cuenta                        SKIP.
      PUT STREAM REPORTE '^FS'                           SKIP.   /* Fin de Campo1 */
      PUT STREAM REPORTE '^FO80,30'                      SKIP.   /* Coordenadas de origen barras */

      PUT STREAM REPORTE '^BY2^BCR,80,Y,N,N'         SKIP.   /* Codigo 128 */
      PUT STREAM REPORTE '^FD>;'.
      PUT STREAM REPORTE x-Codigo SKIP.
      PUT STREAM REPORTE '^FS'                       SKIP.   /* Fin de Campo2 */

      PUT STREAM REPORTE '^LH420,012'                  SKIP.   /* Inicia formato */
      PUT STREAM REPORTE '^FO170,00'                     SKIP.   /* Coordenadas de origen campo1 */
      PUT STREAM REPORTE '^A0R,25,15'                    SKIP.
      PUT STREAM REPORTE '^FD'.
      PUT STREAM REPORTE x-Cuenta                        SKIP.
      PUT STREAM REPORTE '^FS'                           SKIP.   /* Fin de Campo1 */
      PUT STREAM REPORTE '^FO80,30'                      SKIP.   /* Coordenadas de origen barras */

      PUT STREAM REPORTE '^BY2^BCR,80,Y,N,N'         SKIP.   /* Codigo 128 */
      PUT STREAM REPORTE '^FD>;'.
      PUT STREAM REPORTE x-Codigo SKIP.
      PUT STREAM REPORTE '^FS'                       SKIP.   /* Fin de Campo2 */

      PUT STREAM REPORTE '^LH630,012'                  SKIP.   /* Inicia formato */
      PUT STREAM REPORTE '^FO170,00'                     SKIP.   /* Coordenadas de origen campo1 */
      PUT STREAM REPORTE '^A0R,25,15'                    SKIP.
      PUT STREAM REPORTE '^FD'.
      PUT STREAM REPORTE x-Cuenta                        SKIP.
      PUT STREAM REPORTE '^FS'                           SKIP.   /* Fin de Campo1 */
      PUT STREAM REPORTE '^FO80,30'                      SKIP.   /* Coordenadas de origen barras */

      PUT STREAM REPORTE '^BY2^BCR,80,Y,N,N'         SKIP.   /* Codigo 128 */
      PUT STREAM REPORTE '^FD>;'.
      PUT STREAM REPORTE x-Codigo SKIP.
      PUT STREAM REPORTE '^FS'                       SKIP.   /* Fin de Campo2 */

      PUT STREAM REPORTE '^PQ' + TRIM(STRING(x-Copias)) SKIP.  /* Cantidad a imprimir */
      PUT STREAM REPORTE '^PR' + '4'                   SKIP.   /* Velocidad de impresion Pulg/seg */
      PUT STREAM REPORTE '^XZ'                         SKIP.   /* Fin de formato */
  END.


