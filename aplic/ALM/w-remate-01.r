	��V�5�a|5  �              [                                � 357C0110utf-8 MAIN d:\newsie\on_in_co\APLIC\alm\w-remate-01.w,, PROCEDURE Valida,, PROCEDURE Transferencia-Salida,, PROCEDURE Transferencia-Ingreso,, PROCEDURE initializeDataObjects,,INPUT plDeep LOGICAL PROCEDURE Generar-Transferencia,, PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE Carga-Temporal,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE initializeObject,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER        h�              `W             �t h�  ��              ,              8    +   �� �  7   ,� `  8   �� �   D   �� �  E   ,� |  F   �� 0  G   � $  H   � ,
  I   ( �  J       K   & D  L   T; �  M           �F \  4J �  ? $M �'  iSO8859-1                                                                           ,�   , �                                      �                  ��                ��  �3    �3   l�   <�  Ђ         ��  �   0�      <�          �                                             PROGRESS                         l           
    
                    �              �                                                                                                     
               INTEGRAL                         PROGRESS                         �        �          C                      �ɺ[               �l                              �  l                        |  �:     CODCIACODALMTIPMOVCODMOVNRODOCFCHDOCNRORF1NRORF2CODPROCODCLICODVENOBSERVTOTITMCODMONTPOCMBFLGESTALMDESUSUARIONROSERFLGSITHORSALHORRCPFCHANUCODDOCCODREFNROREFCODTRAFLGCBDFCHCBDFLGFACNROFACNOMREFIMPMN1IMPMN2CCOAREAIMPIGVMODADQNRORF3HRADOCLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02ALMFINALLPNDATEUPDATEHOURUPDATEUSERUPDATECROSSDOCKINGALMACENXD                                                                         	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          ,         �          \  H  =   TH     �  �ɺ[�H  %                     �5          �8      �   �         �       X  L  b  X   lb  X  �D  �ɺ[�c  0       X             hI          <M      �   �         �       �  L  <  U   �  �  �!  �ɺ[D�  :       �             �d          (i      �   x             d                                                                                          )             �             �                                                                                          2             �  	   �  �      �                         �#sa            �  �                              �  t	                      �  �	  P�     CODMATDESMATCODMARUNDSTKUNDCMPFACEQUCODCTACODNEWMONVTAPREVTAPREBASAFTIGVVINMN1CODCIAVINMN2CODFAMVCTMN1FCHACTCODPR1CODPR2VCTMN2ARTPROFCHUSALFCHUCMPPMAXMN1PMAXMN2PULTMN1PULTMN2USUARIOFCHINGFCHCESFCHALZCLFMATUNDBASSUBFAMCODBRRCODANTTIPARTFCHPRMDFCHPRMHFCHREAPESMATDETALLECANEMPALMACENESDESMARAFTISCPORISCPORVTATPOMRGCTOLISCTOPRMMRGUTIPORMAXFCHMPREUNDANTPREANTPREACTDSCTOSTPOPROPORIGVCTOTOTTPOSUMCTOUNDORDENORDLISORDTMPTPOARTTPOCMBPPCHR__01CHR__02CHR__03DEC__01DEC__02DEC__03DATE__01DATE__02DATE__03MRGUTI-AMRGUTI-BMRGUTI-CPREOFIUNDAUNDBUNDCFLGINTFLGPRECLASEFCHPROMCATCONTATIPROTDSCTOPROMINFORFLGINFORPROMDIVIPROMFCHDPROMFCHHPROMDTODTOVOLRDTOVOLDUNDALTDSCALTMRGALTPREALTLICENCIAPROMMINDIVIPROMMINFCHDPROMMINFCHHPROMMINDTOCODDIGESAVTODIGESALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKMINSTKMAXSTKREPDESCRIPCION-LARGADESCRIPCION-TECNICASW-WEBWEB-SUBCATEGORIALIBRE_D03LIBRE_D04LIBRE_D05PESOBRUTOPAQUETELARGOALTOANCHOCTOLISMARCOCTOTOTMARCOCODSSFAMCLFESPECIALFLGCOMERCIALLIBRE_C06LIBRE_C07LIBRE_C08LIBRE_C09LIBRE_C10CODIGOPADREFACTORPADREREQUIERESERIALNRREQUIEREDUEDATEDTOVOLPTASAIMPUESTOIMPORTEUNITARIOSINIMPUESTODTOVOLPSINIMPUESTOIMPORTEUNITARIOSINIMPUESTO_AIMPORTEUNITARIOSINIMPUESTO_BIMPORTEUNITARIOSINIMPUESTO_CDTOVOLPIMPUESTOIMPORTEUNITARIOIMPUESTOIMPORTEUNITARIOIMPUESTO_AIMPORTEUNITARIOIMPUESTO_BIMPORTEUNITARIOIMPUESTO_C                                                                      	          
                                                                                                                                                                                                                                      !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2         3          4          5         6          7          8         9          :          ;          <         =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          P          Q          R          S          T          U          V          W          X          Y          Z          [         \         ]         ^         _         `         a 
        b 
        c 
        d 
        e 
        f 
        g         h         i         j         k         l 
        m 
        n 
        o 
        p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          � 
        �          �          � 
        �          �          �          � 
        �          �          �          �            �      �  
    
                  �  H  
                                                                                                     �          
  �  �      @  
    
                  ,  �             �                                                                                          �          
  p  �      �  
    
                  �  �             \                                                                                          �          
    �      �  
    
                  �  L                                                                                                       �          
  �  �      D  
    
                  0  �             �                                                                                          �          
  t  	      �  
    
                  �  �             `                                                                                          	          
           �  
    
                  �  P                                                                                                                 
  �  4      H  
    
                  4  �             �                                                                                          4          
  x  B      �                         �  �             d                                                                                          B            $  O      �                        �  T                                                                                                       O            �  ]      L  
    
                  8                �                                                                                          ]          
  |  k      �  
    
                  �  �             h                                                                                          k          
  (  y      �  
    
                  �  X                                                                                                       y          
  �  �      P                        <                �                                                                                          �            �   �      �                        �  �              l                                                                                           �            ,!  �      �                         �   \!             !                                                                                          �                �      T!                        @!  �!             �!                                                                                          �            $      #  �      #                         C(�\            #  ��                              �  X"                      #  h"  �      CODALMDESCRIPCIONCODCIATDOARTAUTMOVDIRALMHORRECENCALMTELALMCORRSALCORRINGCORRTRFCODDIVCLAVEALMCSGTPOCSGCODCLIFLGREPCAMPO-CALMPRINCIPALALMDESPACHOCAMPO-LOG                                                                        	          
                                                                                                     
                             
         &  !   '$  �      '$                         �ɺ[            0$  �1                              �  �$                      %  �$  ~      CODCIACODDOCNROSERCORRELATIVOCODALMLISTAPRECIOCODDIVPRINTERCODPRONROIMPFCHIMPNROININROFINCODMOVTIPMOVFLGESTFLGCICID_POSID_POS2                                                                        	          
                                                                                                              *  "   K$  �      K$                        �ɺ[            K$  �r                              �  �&                      �'  �&  80     CODCIACODALMTIPMOVCODMOVNRODOCFCHDOCCODMONNROITMTPOCMBCODMATCANDESFACTORPREUNICODUNDIMPCTOAJUSTEALMORIIMPMN1IMPMN2VCTOMN1VCTOMN2STKSUBSTKACTCODAJTNROSERPORDTOIMPDTOIMPLINAFTIGVAFTISCPREBASIMPIGVIMPISCCANDEVDSCTOSIGVMATPRECOSPRELISPESMATCODANTNROANTPOR_DSCTOSFLG_FACTORHRADOCSTKACTCBDSTKSUBCBDVCTOMN1CBDVCTOMN2CBD                                                                      	          
                                                                                                                                                                                                                                       !          "          #          $         %          &          '          (          )          *          +         ,          -          .          /          0          1          d-  #   S$  �      S$                         �ɺ[            S$  .'                              �  �*                      �+  �*  %     CODCIACODALMCODMATUNDVTACODUBISTKACTSTKMINSTKMAXSTKREPSTKINIVINMN1VINMN2VCTMN1VCTMN2FCHINGFCHSALFCHINVSELINVFACEQUDESMATALMDESCODMARCODANTSTKACTCBDLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKCOMPROMETIDOSTOCKMAXSTOCKSEGSTOCKMAXSEG                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          P.  &   �$  �      �$                         �ɺ[            �$  w"                              �  �-                      .  �-        CODCIACODALMTIPMOVCODMOVNRODOC                                                    (3  '      �                               �ɺ[               �l                              �  �.                      p0  �.  �:     CODCIACODALMTIPMOVCODMOVNRODOCFCHDOCNRORF1NRORF2CODPROCODCLICODVENOBSERVTOTITMCODMONTPOCMBFLGESTALMDESUSUARIONROSERFLGSITHORSALHORRCPFCHANUCODDOCCODREFNROREFCODTRAFLGCBDFCHCBDFLGFACNROFACNOMREFIMPMN1IMPMN2CCOAREAIMPIGVMODADQNRORF3HRADOCLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02ALMFINALLPNDATEUPDATEHOURUPDATEUSERUPDATECROSSDOCKINGALMACENXD                                                                         	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;              )       �          X  H  =   TH     �  �ɺ[�H  %                     �5          �8      �                 H�                                              + P�  |�     5  L5  D T�3                          
               Seleccione un almac�n   
             
             
                                         
                                                                                                                D   T   d   �   �   �   �   �   �   �   �       $  4  D      D   T   d   �   �   �   �   �   �   �   �      $  4  D                                                                                                                   	                  
                                                                                                                                                                                                                                                                                                                                                                                                                                                 !                  "                  #                  $                  %                  &                                 ?  ?  ?  $?   ?          (?             <?  D?  L?  \?  T?          `?             t?  |?  �?  �?  �?          �?              �?  �?  �?  �?  �?          �?              �?  �?  @  ,@  @          0@             @@  H@  X@  x@  h@          |@             �@  �@  �@  �@  �@          �@             �@  �@  �@  A  A           A             4A  <A  PA  pA  `A          tA             �A  �A  �A  �A  �A          �A             �A  �A  B  0B  B          4B             TB  \B  pB  �B  �B          �B             �B  �B  �B  �B  �B          �B             C  C  $C  DC  4C          HC              `C  hC  tC  �C  �C          �C              �C  �C  �C  �C  �C          �C              D  D  D  8D  ,D          <D              \D  dD  xD  �D  �D          �D             �D  �D  �D  �D  �D                         �D  �D  �D  E  E                         E   E  (E  <E                              @E  HE  PE  `E  XE                         dE  lE  tE  �E  �E                          �E  �E  �E  �E  �E          �E             �E   F  F  F                              F  $F  ,F  8F                              <F  HF  PF  \F                              `F  lF  tF  �F                              �F  �F  �F  �F                              �F  �F  �F  �F                             �F  �F  �F  G                             G  G   G  ,G                              0G  <G  HG  TG                              XG  hG  |G  �G                             �G  �G  �G  �G                             �G  �G  �G  �G                             �G  �G  H  H                                                                         CodCia  999 Cia Cia 0   C�digo de compa�ia  CodAlm  x(5)    Almac�n Almac�n     C�digo de almac�n   UndVta  X(8)    Unidad de venta Und!Vta     Unidad de venta CodUbi  x(7)    Ubicaci�n   Ubicaci�n       C�digo de ubicaci�n StkAct  (ZZZ,ZZZ,ZZ9.99)    Stock actual    Stock actual    0   Stock actual    StkMin  Z,ZZZ,ZZ9.99    Stock minimo    Stock!minimo    0   Stcok minimo    StkMax  Z,ZZZ,ZZZ,ZZ9.99    Stock maximo    Stock!maximo    0   Stock maximo    StkRep  ZZ,ZZZ,ZZ9.99   Stock de reposicion Stock de!reposicion 0   Stock de reposicion StkIni  (ZZZ,ZZZ,ZZ9.99)    Stock inicial   Stock inicial   0   Stock inicial   VInMn1  (ZZZ,ZZZ,ZZ9.9999)  Valor inicial S/.   Valor inicial S/.   0   Valor inicial moneda nacional   VInMn2  (ZZZ,ZZZ,ZZ9.9999)  Valor inicial US$   Valor inicial US$   0   Valor inicial moneda extranjera VCtMn1  (ZZZ,ZZZ,ZZ9.9999)  Valor costo S/. Valor costo S/. 0   Valor costo moneda nacional VCtMn2  (ZZZ,ZZZ,ZZ9.9999)  Valor costo US$ Valor costo US$ 0   Valor costo moneda extranjera   FchIng  99/99/9999  Fecha ingreso   Fecha!ingreso   ?   Ultima fecha de ingreso FchSal  99/99/9999  Fecha de salida Fecha!salida    ?   Ultima fecha de salida  FchInv  99/99/99    Fecha inventario    Fecha!inventario    ?   Fecha de inventario SelInv  Si/No   Selecc. de inventario   Sel.!inv.   No  Selecci�n de inventario (Si/No) FacEqu  ZZZ,ZZZ,ZZ9.9999    Factor Equivalente  Factor!Equivalente  0   Factor Equivalente  desmat  x(45)   descripcion descripcion     codmat  X(6)    Codigo Articulo Codigo!Articulo     AlmDes  X(70)   Almacen Despacho        CodMar  X(4)    Marca   Marca       CodAnt  X(8)    Codigo Anterior Codigo!Anterior     StkActCbd   (ZZZ,ZZZ,ZZ9.99)    Stock actual    Stock actual    0   Stock actual en almacen Libre_c01   x(60)   Libre_c01       Libre_c02   x(60)   Libre_c02       Libre_c03   x(60)   Libre_c03       Libre_c04   x(60)   Libre_c04       Libre_c05   x(60)   Libre_c05       Libre_d01   ->>>,>>>,>>9.99<<<  Libre_d01   0   Libre_d02   ->>>,>>>,>>9.99<<<  Libre_d02   0   Libre_f01   99/99/99    Libre_f01   ?   Libre_f02   99/99/99    Libre_f02   ?   StkComprometido ->>>,>>,>>9.9999    StkComprometido 0   StockMax    ->>>,>>>,>>9.99 StockMax    0   StockSeg    ->>>,>>>,>>9.99 StockSeg    0   StockMaxSeg ->>>,>>>,>>9.99 StockMaxSeg 0   �   /�  ���&������              ���              ��           �&        �&        �&                �     i  i  i      i  i  i  i      i  i  i     	 	 	 	 	    %   ,   �   3   :   A   H   O   V   ]   d   k   r   y   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �            *  :  C  L                                                                                                                                     	                  
                                                      '                                    &                                                                                                                                                                                                                                                                                                                                                     $                  %                                     !                  "                  #                  (                  )                  *                 +                  ,                  -                  .                  /                  0                  1                                 �U  �U  �U  �U  �U          �U             �U  �U  �U  �U  �U          �U             V  V  V  8V  ,V          <V             PV  XV  \V  �V  tV          �V             �V  �V  �V  �V  �V          �V             �V  �V  W  W  W           W             4W  <W  @W  PW  HW          TW              hW  pW  xW  �W  �W          �W             �W  �W  �W  �W  �W          �W             �W  �W  �W  X  X          X       	      ,X  4X  HX  XX  PX          \X             dX  lX  |X  �X  �X                         �X  �X  �X  �X  �X          �X             �X  �X  Y  (Y  Y                         ,Y  4Y  <Y  HY  DY          LY              dY  lY  �Y  �Y  �Y          �Y             �Y  �Y  �Y  �Y  �Y          �Y              �Y  �Y  �Y  Z  �Y          Z              ,Z  4Z  HZ  hZ  XZ          lZ             �Z  �Z  �Z  �Z  �Z          �Z             �Z  �Z  [  4[  [          8[             \[  d[  x[  �[  �[          �[             �[  �[  �[  \   \          \             ,\  4\  H\  h\  X\          l\             �\  �\  �\  �\  �\          �\              �\  �\  �\  �\  �\                         �\   ]  ]  $]  ]                         (]  0]  8]  P]  D]                         T]  \]  l]  �]  �]                         �]  �]  �]  �]  �]                         �]  �]  �]  �]  �]                          �]  �]  �]  ^   ^                          ^  ^  ^  4^  (^                         8^  @^  H^  X^  P^                         \^  d^  t^  �^  �^                         �^  �^  �^  �^  �^                         �^  �^  �^  �^  �^                         �^   _  _  ,_   _          0_             D_  L_  \_  l_  d_                         p_  x_  �_  �_  �_                          �_  �_  �_  �_  �_          �_              �_  �_  `  `  `                         `  $`  ,`  8`                              <`  D`  L`  d`  X`                         h`  t`  �`  �`  �`          �`             �`  �`  �`  a  �`          a             $a  0a  Da  ta  \a          xa             �a  �a  �a  �a  �a          �a                                                         CodCia  999 Cia Cia 0   C�digo de compa�ia  CodAlm  x(5)    Almac�n Almac�n     C�digo de almac�n   TipMov  X   Tipo de movimiento  Tp.!movmto.     Tipo de movimiento  CodMov  99  C�digo de movimiento    Cd.!Movimto.    0   C�digo de movimiento    NroDoc  999999999   No. documento   Numero de!documento 0   N�mero de documento FchDoc  99/99/9999  Fecha   Fecha!docum TODAY   Fecha de documento  CodMon  9   Moneda  Cod!mon 1   C�digo de moneda    NroItm  >>>>9   Item    Item    0   Numero de Item  TpoCmb  Z,ZZ9.9999  Tipo de cambio  T/Cambio    0   Tipo de cambio  CanDes  (ZZZ,ZZZ,ZZ9.9999)  Cantidad    Cantidad    0   Cantidad despachada Factor  ZZZ,ZZZ,ZZ9.9999    Factor  Factor  0   Factor  PreLis  >>>>,>>9.9999   Precio Lista    Precio Lista    0   PreUni  (Z,ZZZ,ZZ9.9999)    Precio unitario Precio!unitario 0   Precio unitario PreCos  >>>>,>>9.9999   Precio Costo    Precio Costo    0   CodUnd  X(10)   Unidad  Und     Unidad de movimiento    ImpCto  (Z,ZZZ,ZZZ,ZZ9.9999)    Importe Importe 0   Importe total   Ajuste  Si/No   Ajuste  Ajuste  No  Ajuste (Si/No)  AlmOri  x(3)    Almac�n origen  Almac�n!Origen      C�digo de almac�n origen    ImpMn1  (ZZZ,ZZZ,ZZ9.9999)  Importe en S/.  Importe en S/.  0   Importe en moneda nacional  ImpMn2  (ZZZ,ZZZ,ZZ9.9999)  Importe en US$  Importe en US$  0   Importe en moneda extranjera    VctoMn1 (ZZZ,ZZZ,ZZ9.9999)  Valor de costo en S/.   Valor de costo en S/.   0   Valor de costo en moneda nacional   VctoMn2 (ZZZ,ZZZ,ZZ9.9999)  Valor de costo en US$   Valor de costo en US$   0   Valor de costo en moneda extranjera StkSub  (ZZZ,ZZZ,ZZ9.99)    Stock subalmacen    Stock subalmacen    0   Stock en subalmacen StkAct  (ZZZ,ZZZ,ZZ9.99)    Stock actual    Stock actual    0   Stock actual en almacen CodAjt  X   Codigo de ajuste    Cod!Ajt     Codigo de ajuste    codmat  X(6)    Codigo Articulo Codigo Articulo     NroSer  999 Numero Serie    Numero!Serie    0   PorDto  >>9.99  % Dscto.    % Dscto.    0   ImpDto  >,>>>,>>9.9999  Importe Descuento   Importe!Descuento   0   ImpLin  ->>,>>>,>>9.99  Importe Importe 0   AftIgv  Si/No   I.G.V.  I.G.V.  Si  AftIsc  Si/No   I.S.C.  I.S.C.  No  Dsctos  >>>9.99 Descuentos  Descuentos  0   IgvMat  >>>9.99 I.G.V   I.G.V   0   PreBas  >,>>>,>>9.9999  Precio Base Precio Base 0   ImpIgv  >,>>>,>>9.9999  Importe Igv Importe Igv 0   ImpIsc  >,>>>,>>9.9999  Importe Isc Importe Isc 0   CanDev  (ZZZ,ZZZ,ZZ9.9999)  Cantidad    Cantidad    0   Cantidad despachada Pesmat  ->>,>>9.9999    Peso    Peso    0   CodAnt  X(8)    Codigo Anterior Codigo!Anterior     NroAnt  999999  No. documento   Numero !Anterior    0   N�mero de documento Por_Dsctos  ->>9.99 % Dscto % Dscto 0   Flg_Factor  X(1)    Flg_Factor      HraDoc  x(8)    Hora Docu   Hora Docu       StkActCbd   (ZZZ,ZZZ,ZZ9.99)    Stock actual    Stock actual    0   Stock actual en almacen StkSubCbd   (ZZZ,ZZZ,ZZ9.99)    Stock subalmacen    Stock subalmacen    0   Stock en subalmacen VctoMn1Cbd  (ZZZ,ZZZ,ZZ9.9999)  Valor de costo en S/.   Valor de costo en S/.   0   Valor de costo en moneda nacional   VctoMn2Cbd  (ZZZ,ZZZ,ZZ9.9999)  Valor de costo en US$   Valor de costo en US$   0   Valor de costo en moneda extranjera �    1 M�  ���1������     �                          �    �      �    �         �&        �&        �&        �&        �&        �&        �&                �     i  i  i  i  i  i      i  i  i  i  i  i      i  i  i  i  i  i  i      i  i  i  i      i  i  i  i  i  i  i      i  i  i  i  i  i  i      i  i  i 	 i  i  i  i  i     	 	 	 	 		 	 	 	 	- 	    %   ,   ]  d  k  r  y  �  �  �   �  �  �  �  �  �  �  �  �  �  �  �  A   �  �             1  8  ?  F  #  *  �  �  M  �   T  [  f  q  �   x  �  �                                                                                                                                                       	                  
                                                                                                                                                                                                                                                                                                                                                                                                             !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                  :                  ;                                 Ls  Ts  Xs  `s  \s          ds             xs  �s  �s  �s  �s          �s             �s  �s  �s  �s  �s          �s             �s  �s   t  (t  t          ,t             Dt  Lt  Xt  |t  ht          �t             �t  �t  �t  �t  �t          �t             �t  �t  �t  u  �t          u              (u  0u  8u  Xu  Hu          \u             tu  |u  �u  �u  �u          �u             �u  �u  �u  �u  �u          �u             v  v  v  0v   v          4v              Hv  Pv  Xv  �v  pv                          �v  �v  �v  �v  �v          �v              �v  �v  �v   w  �v          w              w  w  $w  @w  4w          Dw              `w  hw  lw  |w  tw          �w              �w  �w  �w  �w  �w          �w             �w  �w  �w  x  �w          x              x  $x  ,x  <x  4x                          @x  Hx  Px  hx  \x                          lx  tx  |x  �x  �x                          �x  �x  �x  �x  �x                          �x  �x  �x  y  �x                         y  y  y  (y   y                          ,y  4y  <y  Ly  Dy                         Py  Xy  `y  py  hy                         ty  |y  �y  �y  �y          �y             �y  �y  �y  �y  �y                          �y  �y  �y  �y  �y                          �y  �y  z   z  z                          $z  ,z  4z  Lz  @z          Pz              lz  tz  |z  �z  �z                          �z  �z  �z  �z  �z          �z             �z  �z  {  ({  {          ,{             L{  P{  X{  p{  h{          t{              �{  �{  �{  �{  �{                          �{  �{  �{  �{  �{                         �{  �{  �{  |   |                           |  (|  0|  P|  @|          T|             l|  t|  ||  �|  �|                          �|  �|  �|  �|                              �|  �|  �|  �|                              �|  �|  �|   }                              }  }  }  $}                              (}  4}  <}  H}                              L}  X}  l}  x}                             |}  �}  �}  �}                             �}  �}  �}  �}                              �}  �}  �}  �}                              �}  ~  ~  ~                               ~  ,~  4~  @~                              D~  P~  X~  d~                              h~  l~  t~  x~                              |~  �~  �~  �~                             �~  �~  �~  �~                             �~  �~  �~  �~                              �~  �~                                    $  ,  8                                                                          CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodAlm  x(5)    Almac�n Almac�n     C�digo de almac�n   TipMov  X   Tipo de movimiento  Tp.!movmto.     Tipo de movimiento  CodMov  99  C�digo de movimiento    C�digo!movimto. 0   C�digo de movimiento    NroDoc  999999999   No. documento   Numero de!documento 0   N�mero de documento FchDoc  99/99/9999  Fecha   Fecha!docum TODAY   Fecha de documento  FchAnu  99/99/9999  Fecha Anulacion Fecha Anulacion ?   Fecha Anulacion Docto.  NroRf1  x(10)   Referencia 1    Referencia 1        N�mero de referencia 1  NroRf2  x(10)   Referencia 2    Referencia 2        N�mero de referencia 2  CodPro  x(11)   Proveedor   C�digo!prov.        C�digo del proveedor    CodCli  x(11)   Cliente C�digo!cliente      C�digo del cliente  CodTra  X(8)    Codigo!Transportista    Codigo!Transportista        CodVen  X(8)    Vendedor    C�digo!vend.        C�digo del vendedor Observ  X(50)   Observaciones   Observaciones       Observaciones   TotItm  >>>>9   Total de Items  Tot.!Itm    0   Total de items del asiento  CodMon  9   Moneda  Cod!mon 1   C�digo de moneda    TpoCmb  Z,ZZ9.9999  Tipo de cambio  T/Cambio    0   Tipo de cambio  AlmDes  x(3)    Almac�n destino Almac�n!Destino     Almac�n destino usuario x(8)    usuario usuario     FlgSit  X(1)    Sit.Transf. Sit.Transf.     HorSal  X(8)    Hora de Salida  Hora de!Salida      HorRcp  X(8)    Hora Recepcion  Hora de!Recepcion       NroSer  999 Numero Serie    Numero!Serie    0   CodDoc  x(3)    Codigo  Codigo      CodRef  x(3)    Codigo  Codigo      NroRef  X(9)    Numero  Numero      FlgEst  X   Flag    Flag        Flag de estado  FlgCbd  X   Flag Contable   Flag        FchCbd  99/99/9999  Fecha   Fecha   ?   FlgFac  Si/No   Flag de Facturado   Flag    No  NroFac  x(10)   No. Factura Documento       Ingrese el No. de Docuemnto NomRef  x(50)   Nombre  Nombre      ImpMn1  (ZZZ,ZZZ,ZZ9.99)    Importe en S/.  Importe en S/.  0   Importe en moneda nacional  ImpMn2  (ZZZ,ZZZ,ZZ9.99)    Importe en US$  Importe en US$  0   Importe en moneda extranjera    cco x(5)    Centro de Costo C.Costo     Centro de Costo Area    X(5)    Area    Area        ImpIgv  ->>,>>>,>>9.99  Importe I.G.V.  Importe I.G.V.  0   ModAdq  X(1)    Modalidad   Modalidad!de Adquisicion        NroRf3  x(10)   Referencia 3    Referencia 3        N�mero de referencia 3  HraDoc  x(8)    Hora Docu   Hora Docu       Libre_c01   x(60)   Libre_c01       Libre_c02   x(60)   Libre_c02       Libre_c03   x(60)   Libre_c03       Libre_c04   x(60)   Libre_c04       Libre_c05   x(60)   Libre_c05       Libre_d01   ->>>,>>>,>>9.99<<<  Libre_d01   0   Libre_d02   ->>>,>>>,>>9.99<<<  Libre_d02   0   Libre_f01   99/99/99    Libre_f01   ?   Libre_f02   99/99/99    Libre_f02   ?   Libre_l01   yes/no  Libre_l01   no  Libre_l02   yes/no  Libre_l02   no  AlmFinal    x(8)    AlmFinal        LPN x(20)   LPN     DateUpdate  99/99/9999  DateUpdate  ?   HourUpdate  x(8)    HourUpdate      UserUpdate  x(15)   UserUpdate      CrossDocking    yes/no  CrossDocking    no  AlmacenXD   x(10)   AlmacenXD       �    0 @�  ���;������     �               �     �                  ��    �          �&        �&        �&        �&        �&        '        '        '        '        !'                �     i  i  i  i  i      i  i  i  i  i  i      i  i  i      i  i  i  i  i  i      i  i  i  i 	 i      i  i 
 i  i  i  i      i  i  i      i  i  i  i      i  i  i      i  i  i  i  i  i     	 	 	 	 	 	
 		 	 	 	( 	 	 	 	7 	8 	    %   ,   ]  d  k  r  �  �  �  �  �  �  �  y  �    �   �  �  �  �  �  �  �    	  �      %  ,  3  �  �  :  >  8  C  J  q  �   �   �   �   �            Q  [  e  n  r  }  �  �  �    ��                                                                              �          ����                            �    ��                   f/    �'    !�    �'  	 ��    �&         �&         �'  & _0    �&         �&   n�    �&  # y�    undefined                                                               �       ��  �   l   ��    ��                  �����               ��r                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D            D          assignFocusedWidget         �      �     �      LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    �      LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          �      LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    �      LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4          LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �          LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    1      LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  >      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  S      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 l      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    w      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 �      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    �      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    �      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    �      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	           LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	          CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER        u   ����  �             d   �           p   �          |   �          �   �          �   �              � ߱            Z   �����
   �p
                     P    q  4  D  �  �       4   �����       o   r       x                              �  �   NA  �   �  �   �  �           $    8    L    `    t    �  `  �  
`  �  $  �    �     �      $  �  $  ���                             
                    � ߱        ��    �  l  �            4   ����                �                      ��                  �  �                  |E^                       �  |  |    �    $      <      4   ����<      $  �  P  ���                       �  @         x              � ߱              �  �  �      �      4   �����      $  �  �  ���                       $  @                       � ߱        assignPageProperty                              �  �      ��                  :  =  �              �G^                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            changePage                              �  �      ��                  ?  @                 ��0                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �  �      ��                  B  D                 @�0                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            constructObject                               �      ��                  F  K  ,              �0                    O   ����    e�          O   ����    R�          O   ����    ��            ��   x             D               �� 
  �             l  
             ��   �             �               �� 
                 �  
         ��                            ����                            createObjects                               �  �      ��                  M  N  �              �*/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �  �      ��                  P  R  �              0+/                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            destroyObject                               �  �      ��                  T  U  �              �;/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �  �      ��                  W  Y  �              ��-                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            initializeObject                                  �      ��                  [  \  ,              X�0                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               $        ��                  ^  _  <              �0                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               $        ��                  a  c  <              0�0                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  T           ��                            ����                            notifyPage                              L  4      ��                  e  g  d              ܽ0                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  |           ��                            ����                            passThrough                             t  \      ��                  i  l  �              \�0                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            removePageNTarget                               �  �      ��                  n  q  �              xK1                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0             �  
             ��                  $           ��                            ����                            selectPage                                      ��                  s  u  4              H10                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  L           ��                            ����                            toolbar                             @   (       ��                  w  y  X               ��-                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  p            ��                            ����                            viewObject                              h!  P!      ��                  {  |  �!              
.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                h"  P"      ��                  ~  �  �"              d.                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �"           ��                            ����                            disablePagesInFolder    
       #      8#          LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder #      d#      �#    "      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  x#      �#      �#    6      HANDLE, getCallerWindow �#       $      0$    I      HANDLE, getContainerMode    $      8$      l$    Y      CHARACTER,  getContainerTarget  L$      x$      �$    j      CHARACTER,  getContainerTargetEvents    �$      �$      �$    }      CHARACTER,  getCurrentPage  �$       %      0%    �      INTEGER,    getDisabledAddModeTabs  %      <%      t%     �      CHARACTER,  getDynamicSDOProcedure  T%      �%      �%  !  �      CHARACTER,  getFilterSource �%      �%      �%  "  �      HANDLE, getMultiInstanceActivated   �%      �%      8&  #  �      LOGICAL,    getMultiInstanceSupported   &      D&      �&  $  �      LOGICAL,    getNavigationSource `&      �&      �&  %        CHARACTER,  getNavigationSourceEvents   �&      �&      '  &  +      CHARACTER,  getNavigationTarget �&      '      H'  '  E      HANDLE, getOutMessageTarget ('      P'      �'  (  Y      HANDLE, getPageNTarget  d'      �'      �'  )  m      CHARACTER,  getPageSource   �'      �'      �'  *  |      HANDLE, getPrimarySdoTarget �'       (      4(  +  �      HANDLE, getReEnableDataLinks    (      <(      t(  ,  �      CHARACTER,  getRunDOOptions T(      �(      �(  -  �      CHARACTER,  getRunMultiple  �(      �(      �(  .  �      LOGICAL,    getSavedContainerMode   �(      �(      0)  /  �      CHARACTER,  getSdoForeignFields )      <)      p)  0  �      CHARACTER,  getTopOnly  P)      |)      �)  1 
 �      LOGICAL,    getUpdateSource �)      �)      �)  2        CHARACTER,  getUpdateTarget �)      �)       *  3        CHARACTER,  getWaitForObject     *      ,*      `*  4  '      HANDLE, getWindowTitleViewer    @*      h*      �*  5  8      HANDLE, getStatusArea   �*      �*      �*  6  M      LOGICAL,    pageNTargets    �*      �*      +  7  [      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �*      L+      |+  8  h      LOGICAL,INPUT h HANDLE  setCallerProcedure  \+      �+      �+  9  x      LOGICAL,INPUT h HANDLE  setCallerWindow �+      �+      ,  :  �      LOGICAL,INPUT h HANDLE  setContainerMode    �+      (,      \,  ;  �      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  <,      �,      �,  <  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �,      �,      -  =  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �,      (-      `-  >  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  @-      �-      �-  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �-      �-      .  @  �      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �-      8.      l.  A  	      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   L.      �.      �.  B  	      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �.      �.      4/  C  9	      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource /      d/      �/  D  S	      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   x/      �/      �/  E  g	      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �/      0      P0  F  �	      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget 00      p0      �0  G  �	      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �0      �0      �0  H  �	      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �0      1      H1  I  �	      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget (1      h1      �1  J  �	      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    |1      �1      �1  K  �	      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �1      (2      X2  L  �	      LOGICAL,INPUT phObject HANDLE   setRunDOOptions 82      x2      �2  M  �	      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �2      �2      �2  N  
      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �2       3      X3  O  
      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields 83      �3      �3  P  4
      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �3      �3      4  Q 
 H
      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �3      04      `4  R  S
      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget @4      �4      �4  S  c
      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    �4      �4      5  T  s
      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �4      ,5      d5  U  �
      LOGICAL,INPUT phViewer HANDLE   getObjectType   D5      �5      �5  V  �
      CHARACTER,  setStatusArea   �5      �5      �5  W  �
      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �6  �6      ��                  �  �  �6               M�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �7  �7      ��                  �     �7              dX                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �8  �8      ��                      �8              �Z                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �9  �9      ��                      �9              �[                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �:  �:      ��                    
  �:              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �:           ��                            ����                            getAllFieldHandles  �5      P;      �;  X  �
      CHARACTER,  getAllFieldNames    d;      �;      �;  Y  �
      CHARACTER,  getCol  �;      �;      �;  Z  �
      DECIMAL,    getDefaultLayout    �;      <      8<  [  �
      CHARACTER,  getDisableOnInit    <      D<      x<  \  �
      LOGICAL,    getEnabledObjFlds   X<      �<      �<  ]        CHARACTER,  getEnabledObjHdls   �<      �<      �<  ^        CHARACTER,  getHeight   �<      =      0=  _ 	 &      DECIMAL,    getHideOnInit   =      <=      l=  `  0      LOGICAL,    getLayoutOptions    L=      x=      �=  a  >      CHARACTER,  getLayoutVariable   �=      �=      �=  b  O      CHARACTER,  getObjectEnabled    �=      �=      ,>  c  a      LOGICAL,    getObjectLayout >      8>      h>  d  r      CHARACTER,  getRow  H>      t>      �>  e  �      DECIMAL,    getWidth    |>      �>      �>  f  �      DECIMAL,    getResizeHorizontal �>      �>      ?  g  �      LOGICAL,    getResizeVertical   �>       ?      T?  h  �      LOGICAL,    setAllFieldHandles  4?      `?      �?  i  �      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    t?      �?      �?  j  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �?      @      <@  k  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    @      `@      �@  l  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   t@      �@      �@  m  �      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �@      A      8A  n        LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout A      \A      �A  o        LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal lA      �A      �A  p  -      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �A      B      DB  q  A      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated $B      lB      �B  r  S      LOGICAL,    getObjectSecured    �B      �B      �B  s  g      LOGICAL,    createUiEvents  �B      �B      C  t  x      LOGICAL,    bindServer                              �C  �C      ��                  �  �  �C              tX                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �D  �D      ��                  �  �  �D              �c                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �E  �E      ��                  �  �  �E              �f                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �F  �F      ��                  �  �  �F              xi                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �G  �G      ��                  �  �  �G               j                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �H  �H      ��                  �  �  �H              �j                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �I  �I      ��                  �     �I              �m                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 J  
         ��                            ����                            startServerObject                               K  �J      ��                      ,K              ,r                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                L   L      ��                      0L              �r                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  HL           ��                            ����                            getAppService   �B      �L      �L  u  �      CHARACTER,  getASBound  �L      �L      M  v 
 �      LOGICAL,    getAsDivision   �L      $M      TM  w  �      CHARACTER,  getASHandle 4M      `M      �M  x  �      HANDLE, getASHasStarted lM      �M      �M  y  �      LOGICAL,    getASInfo   �M      �M      �M  z 	 �      CHARACTER,  getASInitializeOnRun    �M      N      @N  {  �      LOGICAL,    getASUsePrompt   N      LN      |N  |  �      LOGICAL,    getServerFileName   \N      �N      �N  }  �      CHARACTER,  getServerOperatingMode  �N      �N       O  ~  
      CHARACTER,  runServerProcedure  �N      O      @O    !      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService    O      �O      �O  �  4      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �O      �O      P  �  B      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �O      0P      \P  �  P      LOGICAL,INPUT phASHandle HANDLE setASInfo   <P      |P      �P  � 	 \      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �P      �P       Q  �  f      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �P      $Q      TQ  �  {      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   4Q      tQ      �Q  �  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �Q      �Q      R  �  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �R  �R      ��                  �  �  �R              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $S             �R  
             ��   LS             S               �� 
                 @S  
         ��                            ����                            addMessage                              8T   T      ��                  �  �  PT              ,�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �T             hT               ��   �T             �T               ��                  �T           ��                            ����                            adjustTabOrder                              �U  �U      ��                  �  �  �U              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  V             �U  
             �� 
  @V             V  
             ��                  4V           ��                            ����                            applyEntry                              ,W  W      ��                  �  �  DW              4�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  \W           ��                            ����                            changeCursor                                XX  @X      ��                  �  �  pX              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �X           ��                            ����                            createControls                              �Y  lY      ��                  �  �  �Y              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �Z  pZ      ��                  �  �  �Z              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �[  t[      ��                  �  �  �[              P�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �\  �\      ��                  �  �  �\              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �]  �]      ��                  �  �  �]              X�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �^  �^      ��                  �  �  �^              X�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �_  �_      ��                  �  �  �_              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �`  �`      ��                  �  �  �`              `�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  a             �`  
             ��   4a              a               ��   \a             (a               ��                  Pa           ��                            ����                            modifyUserLinks                             Lb  4b      ��                       db              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �b             |b               ��   �b             �b               �� 
                 �b  
         ��                            ����                            removeAllLinks                              �c  �c      ��                      �c              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �d  �d      ��                  	    �d              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  ,e             �d  
             ��   Te              e               �� 
                 He  
         ��                            ����                            repositionObject                                Hf  0f      ��                      `f              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �f             xf               ��                  �f           ��                            ����                            returnFocus                             �g  �g      ��                      �g              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �g  
         ��                            ����                            showMessageProcedure                                �h  �h      ��                      �h                                   O   ����    e�          O   ����    R�          O   ����    ��            ��   0i             �h               ��                  $i           ��                            ����                            toggleData                              j  j      ��                      4j              (                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  Lj           ��                            ����                            viewObject                              Dk  ,k      ��                  !  "  \k              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �Q      �k      �k  � 
       LOGICAL,    assignLinkProperty  �k      �k       l  �        LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages    l      xl      �l  �        CHARACTER,  getChildDataKey �l      �l      �l  �  -      CHARACTER,  getContainerHandle  �l      �l      $m  �  =      HANDLE, getContainerHidden  m      ,m      `m  �  P      LOGICAL,    getContainerSource  @m      lm      �m  �  c      HANDLE, getContainerSourceEvents    �m      �m      �m  �  v      CHARACTER,  getContainerType    �m      �m      $n  �  �      CHARACTER,  getDataLinksEnabled n      0n      dn  �  �      LOGICAL,    getDataSource   Dn      pn      �n  �  �      HANDLE, getDataSourceEvents �n      �n      �n  �  �      CHARACTER,  getDataSourceNames  �n      �n      o  �  �      CHARACTER,  getDataTarget   �n      (o      Xo  �  �      CHARACTER,  getDataTargetEvents 8o      do      �o  �  �      CHARACTER,  getDBAware  xo      �o      �o  � 
       LOGICAL,    getDesignDataObject �o      �o      p  �        CHARACTER,  getDynamicObject    �o      p      Pp  �  *      LOGICAL,    getInstanceProperties   0p      \p      �p  �  ;      CHARACTER,  getLogicalObjectName    tp      �p      �p  �  Q      CHARACTER,  getLogicalVersion   �p      �p      q  �  f      CHARACTER,  getObjectHidden �p      $q      Tq  �  x      LOGICAL,    getObjectInitialized    4q      `q      �q  �  �      LOGICAL,    getObjectName   xq      �q      �q  �  �      CHARACTER,  getObjectPage   �q      �q      r  �  �      INTEGER,    getObjectParent �q      r      Lr  �  �      HANDLE, getObjectVersion    ,r      Tr      �r  �  �      CHARACTER,  getObjectVersionNumber  hr      �r      �r  �  �      CHARACTER,  getParentDataKey    �r      �r      s  �  �      CHARACTER,  getPassThroughLinks �r      s      Ls  �        CHARACTER,  getPhysicalObjectName   ,s      Xs      �s  �        CHARACTER,  getPhysicalVersion  ps      �s      �s  �  ,      CHARACTER,  getPropertyDialog   �s      �s      t  �  ?      CHARACTER,  getQueryObject  �s      t      Lt  �  Q      LOGICAL,    getRunAttribute ,t      Xt      �t  �  `      CHARACTER,  getSupportedLinks   ht      �t      �t  �  p      CHARACTER,  getTranslatableProperties   �t      �t      u  �  �      CHARACTER,  getUIBMode  �t      u      Hu  � 
 �      CHARACTER,  getUserProperty (u      Tu      �u  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    du      �u      �u  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles �u      v      8v  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    v      \v      �v  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry lv      �v      �v  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �v      `w      �w  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    pw      �w      �w  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  �w      x      <x  �        CHARACTER,  setChildDataKey x      Hx      xx  �        LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  Xx      �x      �x  �  +      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  �x      �x      (y  �  >      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    y      Hy      �y  �  Q      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled dy      �y      �y  �  j      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   �y      z      4z  �  ~      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents z      Tz      �z  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  hz      �z      �z  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   �z      {      <{  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents {      `{      �{  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  t{      �{      �{  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject �{      |      8|  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    |      `|      �|  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   t|      �|      �|  �        LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    �|      }      D}  �        LOGICAL,INPUT c CHARACTER   setLogicalVersion   $}      `}      �}  �  0      LOGICAL,INPUT cVersion CHARACTER    setObjectName   t}      �}      �}  �  B      LOGICAL,INPUT pcName CHARACTER  setObjectParent �}      ~      8~  �  P      LOGICAL,INPUT phParent HANDLE   setObjectVersion    ~      X~      �~  �  `      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    l~      �~      �~  �  q      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks �~            D  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   $      d      �  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  |      �      �  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      �      D�  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   $�      l�      ��  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      Ā       �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      $�      P�  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty 0�      p�      ��  �        LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      �  �        LOGICAL,INPUT pcMessage CHARACTER   Signature   �      0�      \�  � 	 "      CHARACTER,INPUT pcName CHARACTER    T�    8  ��  �      T      4   ����T                (�                      ��                  9  f                  <I                       9  ��        :  D�  ��      d      4   ����d                Ѓ                      ��                  ;  e                  �I                       ;  T�  Є    R  �  h�      x      4   ����x                x�                      ��                  ^  `                  $J                       ^  ��         _                                  L     
                    � ߱        ��  $  b  ��  ���                           $  d  (�  ���                       �                         � ߱        `�    j  p�  �      �      4   �����                ��                      ��                  k  /	                  �J                       k  ��  0�  o   n      ,                                 ��  $   o  \�  ���                         @                       � ߱        ��  �   p  <      ��  �   q  �      Ć  �   s  $      ؆  �   u  �      �  �   w         �  �   y  �      �  �   z  �      (�  �   {  8      <�  �   ~  �      P�  �   �   	      d�  �   �  �	      x�  �   �  
      ��  �   �  �
      ��  �   �  �
      ��  �   �  L      ȇ  �   �  �      ܇  �   �  �      ��  �   �  p      �  �   �  �      �  �   �         ,�  �   �  �      @�  �   �        T�  �   �  �      h�  �   �         |�  �   �  |      ��  �   �  �      ��  �   �  d      ��  �   �  �      ̈  �   �        ��  �   �  P      �  �   �  �      �  �   �         �  �   �  <      0�  �   �  x      D�  �   �  �      X�  �   �  0      l�  �   �  l      ��  �   �  �      ��  �   �  �      ��  �   �         ��  �   �  \      Љ  �   �  �      �  �   �  �      ��  �   �            �   �  L                      $�          ��  x�      ��                  V	  �	  ��              �b                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                8       	       	       H                         � ߱        P�  $ j	  ��  ���                           O   �	  ��  ��  �               ��          ��  ��    ��                                             ��                            ����                                �5      �      h�     6     ċ                      V ��  �
                      �    �	  |�  ��      �      4   �����                �                      ��                  �	  +
                  �c                       �	  ��  �  �   �	  �      0�  �   �	  h      D�  �   �	  �      X�  �   �	  `      l�  �   �	  �      ��  �   �	  X      ��  �   �	  �      ��  �   �	  H      ��  �   �	  �      Ѝ  �   �	  @      �  �   �	  �      ��  �   �	  0      �  �   �	  �          �   �	  (      ��    6
  <�  ��      �      4   �����                Ȏ                      ��                  7
  �
                  ��                       7
  L�  ܎  �   9
  �      ��  �   :
  l      �  �   ;
  �      �  �   <
  \       ,�  �   =
  �       @�  �   >
  D!      T�  �   ?
  �!      h�  �   @
  4"      |�  �   A
  �"      ��  �   B
  #      ��  �   C
  �#      ��  �   D
  $      ̏  �   E
  �$      ��  �   F
  �$      �  �   G
  x%      �  �   H
  �%      �  �   I
  p&      0�  �   J
  �&      D�  �   K
  h'      X�  �   L
  �'      l�  �   M
  `(      ��  �   N
  �(      ��  �   O
  X)      ��  �   P
  �)      ��  �   Q
  P*      А  �   R
  �*      �  �   S
  H+          �   T
  �+      �    �
  �  ��      ,,      4   ����,,                ��                      ��                  �
  �                   �                       �
  $�  ��  �   �
  �,      ȑ  �   �
  -      ܑ  �   �
  �-      �  �   �
  �-      �  �   �
  l.      �  �   �
  �.      ,�  �   �
  T/      @�  �   �
  �/      T�  �   �
  0      h�  �   �
  @0      |�  �   �
  |0      ��  �   �
  �0      ��  �   �
  d1      ��  �   �
  �1      ̒  �   �
  T2      ��  �   �
  �2      ��  �   �
  <3      �  �   �
  �3      �  �   �
  44      0�  �   �
  p4      D�  �   �
  �4      X�  �   �
  X5      l�  �   �
  �5      ��  �   �
  6      ��  �   �
  D6      ��  �   �
  �6      ��  �   �
  �6      Г  �   �
  87      �  �   �
  t7      ��  �   �
  �7      �  �   �
  �7       �  �   �
  (8      4�  �   �
  d8      H�  �   �
  �8      \�  �   �
  9      p�  �   �
  P9      ��  �   �
  �9      ��  �   �
  �9      ��  �      :      ��  �     @:      Ԕ  �     |:      �  �     �:      ��  �     d;      �  �     �;      $�  �     L<      8�  �     �<      L�  �     D=      `�  �   	  �=      t�  �   
  <>      ��  �     �>      ��  �     4?      ��  �     p?      ĕ  �     �?      ؕ  �     (@      �  �     d@       �  �     �@          �     A      l�  $  �  @�  ���                       |A     
  
       
           � ߱        �    �  ��  ��      �A      4   �����A      /   �  Ė     Ԗ                          3   �����A            ��                      3   �����A  X�    �   �  ��  ��  �A      4   �����A  	              ��                      ��             	     �  W                  �                       �  0�  ��  �   �  4B      �  $  �  �  ���                       `B     
                    � ߱        ,�  �   �  �B      ��  $   �  X�  ���                       �B  @         �B              � ߱        @�  $  �  ��  ���                       �B                         � ߱        pC     
                �C       	       	       <E  @        
 �D              � ߱        Й  V   �  ܘ  ���                        HE                     |E                     �E                         � ߱        `�  $    l�  ���                       xF     
                �F       	       	       DH  @        
 H              � ߱        �  V     ��  ���                        PH     
                �H       	       	       J  @        
 �I              � ߱            V   ;  ��  ���                        
              P�                      ��             
     Y  �                  ��                       Y  �  (J     
                �J       	       	       �K  @        
 �K          XL  @        
 L          �L  @        
 xL          M  @        
 �L              � ߱            V   n  ��  ���                        adm-clone-props �  |�              �     7     `                          \  l                      start-super-proc    ��  �  �           �     8                                  �                      �      t�  ��      �P      4   �����P      /     ��     ��                          3   �����P            ��                      3   �����P  H�  $  )  �  ���                       �P                         � ߱        �    9  d�  ��  ��  Q      4   ����Q                T�                      ��                  :  >                  X~                       :  t�  $Q                     8Q                     LQ                         � ߱            $  ;  �  ���                             ?  ��  ؟      dQ      4   ����dQ  �Q                         � ߱            $  @  ��  ���                        �    G   �  0�  ��  �Q      4   �����Q      $  H  \�  ���                       �Q                         � ߱            �   e  �Q      R     
                �R       	       	       �S  @        
 �S              � ߱        ,�  V   y  ��  ���                        @�  �   �  �S      ء    .  \�  l�      $T      4   ����$T      /   /  ��     ��                          3   ����4T            ȡ                      3   ����TT  ��  $  3  �  ���                       pT                         � ߱        �T     
                U       	       	       hV  @        
 (V              � ߱        ��  V   =  0�  ���                        ��    �  ܢ  X�      tV      4   ����tV                h�                      ��                  �  �                  �\                       �  �      g   �  ��         \�D�                           H�          �   �      ��                  �      0�              ]                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  t�     ��  �V                      3   �����V  ��     
   ��                      3   �����V         
   Ԥ                      3   �����V    ��                              ��        �                  ����                                        ��              9      �                      g                               ��  g   �  ��          \�	L�                           ��          P�  8�      ��                  �  �  h�              �]                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  �V                      3   �����V            ܦ                      3   �����V    ��                              ��        �                  ����                                        ̥              :      �                      g                               ��  g   �  ��          \�	T�                           ��          X�  @�      ��                  �  �  p�              T^                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     Ĩ  W                      3   �����V            �                      3   ����W    ��                              ��        �                  ����                                        ԧ              ;      ��                      g                               �    �  ̩  H�      8W      4   ����8W                X�                      ��                  �  �                  (�                       �  ܩ  Ī  /   �  ��     ��                          3   ����HW            ��                      3   ����hW  ��  /  �  �      �  �W                      3   �����W  0�     
    �                      3   �����W  `�        P�                      3   �����W  ��        ��                      3   �����W            ��                      3   �����W  �    �  ܫ  �      X      4   ����X      /  �  �     (�  �X                      3   ����xX  X�     
   H�                      3   �����X  ��        x�                      3   �����X  ��        ��                      3   �����X            ج                      3   �����X        �  �  �       Y      4   ���� Y      /  �  @�     P�  TY                      3   ����4Y  ��     
   p�                      3   ����\Y  ��        ��                      3   ����dY  �        Э                      3   ����xY             �                      3   �����Y  а    �  ,�  ��      �Y      4   �����Y                ��                      ��                                       8�                          <�      g     Ю         \�t�        �Y                  ��          h�  P�      ��                        ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /    į     ԯ  �Y                      3   �����Y  �     
   ��                      3   �����Y         
   $�                      3   ���� Z    ��                            ����                                        �              <      4�                      g                               h�       Z                                     Z     
                �Z       	       	       �[  @        
 �[              � ߱        ��  V   u  �  ���                        �[     
                x\       	       	       �]  @        
 �]              � ߱        $�  V   �  ��  ���                        ��    �  @�  P�      �]      4   �����]      $   �  |�  ���                       <^  @         (^              � ߱        |�  g   �  ��         \� �        P^  \� �        \^                  ��          l�  T�      ��                  �  �  ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �  ��  ȳ      h^      4   ����h^      O  �  ������  |^    ��                            ����                                        �              =      �                      g                               (�  g   �  ��         \6̵         �^                  \�          ,�  �      ��                  �    D�              �                    O   ����    e�          O   ����    R�          O   ����    ��      t�      �^  }          O    ������  �^    ��                            ����                                        ��              >      ��                      g                                �  g     @�         \"��                           �          ض  ��      ��                      �              `�^                    O   ����    e�          O   ����    R�          O   ����    ��          /     4�                                 3   �����^    ��                              ��        �                  ����                                        T�              ?      D�                      g                               ع  g     �         \"|�                           �          ��  ��      ��                      ȸ              ��^                    O   ����    e�          O   ����    R�          O   ����    ��          /     �                                 3   �����^    ��                              ��        �                  ����                                        ,�              @      �                      g                               ̻  g   "  �         \4p�                            �          ��  p�      ��                  #  %  ��              H�^                    O   ����    e�          O   ����    R�          O   ����    ��                                 � ߱            $   $  ��   �                         ��                              ��        �                  ����                                        �              A      �                      g                                     A  �  d�      _      4   ����_                ؼ                      ��                  A  m                  �F]                       A  ��  _  @                     @_  @         ,_          h_  @         T_              � ߱        �  $   B  t�  ���                        �  g   H  �         \n��      }                      �          ��  ��      ��                  I  M  ̽              @�                    O   ����    e�          O   ����    R�          O   ����    ��       �  /  J  �                                 3   ����t_        K  <�  L�      �_      4   �����_      O  L  ������  �_    ��                            ����                                        0�              B      d�                      g                               ��  g   R  �         \!x�         �_                  �          ��  ��      ��                  R  T  ȿ              ��                    O   ����    e�          O   ����    R�          O   ����    ��      �_  @                         � ߱            $  S  �  ���                         ��                            ����                                        ,�              C      8�                      g                               �  /   W   �                                 3   �����_        ^  ,�  ��      `      4   ����`                $�                      ��                  ^  k                  p�                       ^  <�                d�          L�  4�      ��                 b  i                  H�                       b  ��      O   b    ��          O   b    ��      ��  /   f  ��                                 3   ���� `        g  ��  ��      @`      4   ����@`      k   h  ��              }       n        �   adm-create-objects  ��   �                      D      �                               "                     Carga-Temporal  �  p�          �  �    E     �             �          �  �#                     disable_UI  ��  ��                      F      <                              �#  
                   enable_UI   ��  D�                      G      �                           $  	                   exitObject  P�  ��                      H      �                               $  
                   Generar-Transferencia   ��  �                      I      �	             
              \$                     initializeDataObjects   ,�  ��  �           �    $ J     X                          T  �$                     Transferencia-Ingreso   ��  ��              �    % K     X                          T  n%                     Transferencia-Salida    �  p�          �  �  * ( L    �  |                      �  �%                     Valida  ��  ��              �
    + M                                 �&                      ����   �  $  �Seleccione un almac�n���  �           m      	 ��  8   ����   ��  8   ����   �    ��  8   ����   ��  8   ����   ��  8   ����'   �  8   ����'   �  8   ����&   ,�  8   ����&   \�  &  <�  8   ����   L�  8   ����   d�  8   ����#   t�  8   ����#   ��  #  ��  8   ����"   ��  8   ����"   ��  8   ����!   ��  8   ����!   ��  8   ����    ��  8   ����    ��     ��  8   ����	   �  8   ����	   �  	      8   ����       8   ����             ,�  8�      toggleData  ,INPUT plEnabled LOGICAL    �  d�  |�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  T�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��  �      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  D�  P�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE 4�  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  0�  D�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER     �  ��  ��      hideObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  �  �      displayLinks    ,   ��  ,�  <�      createControls  ,   �  P�  `�      changeCursor    ,INPUT pcCursor CHARACTER   @�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    |�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  ,�  8�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER �  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  $�  8�      startServerObject   ,   �  L�  \�      runServerObject ,INPUT phAppService HANDLE  <�  ��  ��      restartServerObject ,   x�  ��  ��      initializeServerObject  ,   ��  ��  ��      disconnectObject    ,   ��  �  �      destroyServerObject ,   ��  ,�  8�      bindServer  ,   �  L�  \�      processAction   ,INPUT pcAction CHARACTER   <�  ��  ��      enableObject    ,   x�  ��  ��      disableObject   ,   ��  ��  ��      applyLayout ,   ��  ��  ��      viewPage    ,INPUT piPageNum INTEGER    ��  (�  4�      viewObject  ,   �  H�  P�      toolbar ,INPUT pcValue CHARACTER    8�  |�  ��      selectPage  ,INPUT piPageNum INTEGER    l�  ��  ��      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  �  �      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  X�  d�      notifyPage  ,INPUT pcProc CHARACTER H�  ��  ��      initPages   ,INPUT pcPageList CHARACTER |�  ��  ��      initializeVisualContainer   ,   ��  ��  �      initializeObject    ,   ��  �  (�      hidePage    ,INPUT piPageNum INTEGER    �  T�  d�      destroyObject   ,   D�  x�  ��      deletePage  ,INPUT piPageNum INTEGER    h�  ��  ��      createObjects   ,   ��  ��  ��      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  X�  d�      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  H�  ��  ��      changePage  ,   ��  ��  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 u%     adecomm/as-utils.w 
"   
   �    }        �
"     
   "      "      " 	     " 	     "          �     }        �G� �   �G%              � �  +   %              %        %        %       %        %       %               %               %               %              %              %              %               %              
�        
"   
 u
�    
"   
 u
"   
 �    �        $     �        0    
"   
   �        l         �     }        �%              
"   
 u
"   
 �    �        �     �        �    
"   
   �                 �     }        �%              � 
"    
 �%              � �  �         �      T     @     $              
�    � ,   �     
"   
 � ,   �     
�             �G                      
�            � .   
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 N�           0    1� >  
 N� I   �%               o%   o           � N    N
"   
 N�           �    1� O   N� I   �%               o%   o           � ]   N
"   
 N�               1� d  
 N� I   �%               o%   o           � o   N
"   
 N�           �    1� {   N� I   �%               o%   o           � �   N
"   
 N�                1� �   N� I   �%               o%   o           � �   N
"   
 N�           t    1� �   N� �   �%               o%   o           %               
"   
 ��          �    1� �   �� �     
"   
 N�           ,    1� �   N� I   �%               o%   o           � �  e N
"   
 N�           �    1� Z   N� I   �%               o%   o           � i  [ N
"   
 N�           	    1� �   N� �   �%               o%   o           %               
"   
 N�           �	    1� �   N� �   �%               o%   o           %               
"   
 N�           
    1� �   N� �   �%               o%   o           %              
"   
 ��          �
    1� �   �� �     
"   
 N�           �
    1�   
 N� �   �%               o%   o           %               
"   
 N�           @    1�    N� I   �%               o%   o           � N    N
"   
 ��          �    1�    �� �     
"   
 N�           �    1� &   N� I   �%               o%   o           � <  t N
"   
 ��          d    1� �  
 �� �     
"   
 N�           �    1� �   N� I   �%               o%   o           � �  � N
"   
 N�               1� Z   N� I   �%               o%   o           � N    N
"   
 N�           �    1� q  
 N� |   �%               o%   o           %               
"   
 �               1� �   � �   �%               o%   o           %               
"   
 �           �    1� �   � I   �%               o%   o           � N    
"   
 �           �    1� �   � I   �%               o%   o           o%   o           
"   
 �           p    1� �  
 � I   �%               o%   o           � N    
"   
 �           �    1� �   � �  	 �%               o%   o           � �  / 
"   
 ��          X    1� �   �� �  	   
"   
 �           �    1�    � �  	 �o%   o           o%   o           � N    
"   
 ��              1� $   �� �  	   
"   
 u�           D    1� 3   u� �  	 �o%   o           o%   o           � N    u
"   
 ��          �    1� C   �� �     
"   
 ��          �    1� Q   �� �  	   
"   
 ��          0    1� ^   �� �  	   
"   
 ��          l    1� k   �� �  	   
"   
 �           �    1� y   � �   �o%   o           o%   o           %              
"   
 ��          $    1� �   �� �  	   
"   
 ��          `    1� �  
 �� �     
"   
 ��          �    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��              1� �   �� �  	   
"   
 ��          P    1� �   �� �  	   
"   
 ��          �    1� �  	 �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��              1�    �� �  	   
"   
 �           @    1� %   � I   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
 �(�  L ( l       �            �� 1   � P   �            �@    
� @  , 
�            �� :     p�               �L
�    %              � 8      ,    � $         � A          
�    � [     
"   
 �� @  , 
�       <    �� d  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� ^  
 � I   �%               o%   o           � N    
"   
 �           \    1� i  
 � I   �%               o%   o           o%   o           
"   
 �           �    1� t   � �   �%               o%   o           o%   o           
"   
 �           T    1� }   � �   �%               o%   o           %               
"   
 �           �    1� �   � �   �%               o%   o           %               
"   
 ^�           L    1� �   ^� I   �%               o%   o           � N    
"   
 �           �    1� �   � �   �%               o%   o           %              
"   
 �           <    1� �   � �   �%               o%   o           o%   o           
"   
 �           �    1� �   � I   �%               o%   o           o%   o           
"   
 �           4    1� �  	 � I   �%               o%   o           � N    
"   
 �           �    1� �   � I   �%               o%   o           o%   o           
"   
 �           $    1� �   � I   �%               o%   o           o%   o           
"   
 �           �    1� �   � �   �%               o%   o           %               
"   
 �               1� 	   � �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1�    � �  	 �%               o%   o           � N    
"   
 �           `    1� "   � �  	 �%               o%   o           � N    
"   
 �           �    1� 0   � �   �%               o%   o           %               
"   
 ^�           P     1� >   ^� �  	 �%               o%   o           � N    
"   
 �           �     1� M   � �  	 �%               o%   o           � N    ^
"   
 �           8!    1� [   � �   �%               o%   o           %               
"   
 �           �!    1� i   � �  	 �%               o%   o           � N    
"   
 �           ("    1� x   � �  	 �%               o%   o           � N    
"   
 �           �"    1� �   � �  	 �%               o%   o           � N    
"   
 �           #    1� �   � �  	 �%               o%   o           o%   o           
"   
 �           �#    1� �   � �  	 �%               o%   o           � N    
"   
 ^�            $    1� �   ^� �  	 �%               o%   o           � N    
"   
 �           t$    1� �  	 � �   �%               o%   o           %               
"   
 �           �$    1� �   � �   �%               o%   o           %               
"   
 �           l%    1� �   � �   �%               o%   o           o%   o           
"   
 �           �%    1� �   � �   �%               o%   o           o%   o           
"   
 �           d&    1� �   � �   �%               o%   o           %               
"   
 �           �&    1�    � �   �%               o%   o           %               
"   
 �           \'    1�    � �   �%               o%   o           %               
"   
 ^�           �'    1� (   ^� 4   �%               o%   o           %       
       
"   
 ^�           T(    1� <   ^� 4   �%               o%   o           o%   o           
"   
 �           �(    1� H   � 4   �%               o%   o           %              
"   
 �           L)    1� T   � 4   �%               o%   o           o%   o           
"   
 �           �)    1� `   � 4   �%               o%   o           %              
"   
 �           D*    1� m   � 4   �%               o%   o           o%   o           
"   
 �           �*    1� z   � 4   �%               o%   o           %              
"   
 �           <+    1� �   � 4   �%               o%   o           o%   o           
"   
 ^�           �+    1� �   ^� �  	 �%               o%   o           � N    P �L 
�H T   %              �     }        �GG %              
"   
 �           �,    1� �   � |   �%               o%   o           %               
"   
 �           �,    1� �   � |   �%               o%   o           o%   o           
"   
 �           x-    1� �   � I   �%               o%   o           � N    
"   
 �           �-    1� �   � I   �%               o%   o           � �  - 
"   
 �           `.    1�    � I   �%               o%   o           � N    
"   
 �           �.    1�    � I   �%               o%   o           � <   
"   
 ��          H/    1� Z   �� �     
"   
 �           �/    1� k   � I   �%               o%   o           � N    
"   
 ��          �/    1� w  
 �� �     
"   
 ��          40    1� �   �� �     
"   
 �           p0    1� �   � �  	 �%               o%   o           � N    
"   
 �           �0    1� �   � I   �%               o%   o           � N    
"   
 �           X1    1� �   � �   �%               o%   o           o%   o           
"   
 �           �1    1� �   � I   �%               o%   o           � �  ! 
"   
 �           H2    1� �   � I   �%               o%   o           � N    
"   
 ^�           �2    1� �   ^� I   �%               o%   o           �    
"   
 ^�           03    1�   	 ^� |   �%               o%   o           o%   o           
"   
 �           �3    1� $   � �   �%               o%   o           %               
"   
 ��          (4    1� 0   �� �     
"   
 �           d4    1� >   � I   �%               o%   o           � R   
"   
 �           �4    1� a   � �  	 �%               o%   o           � N    
"   
 �           L5    1� n   � �  	 �%               o%   o           � N    
"   
 ��          �5    1� ~   �� �     
"   
 ��          �5    1� �   �� �  	   
"   
 ^�           86    1� �   ^� �   �o%   o           o%   o           %               
"   
 ��          �6    1� �   �� �     
"   
 ��          �6    1� �   �� �  	   
"   
 ��          ,7    1� �   �� �  	   
"   
 ��          h7    1� �   �� �  	   
"   
 ��          �7    1�    �� �  	   
"   
 ��          �7    1�    �� �  	   
"   
 ��          8    1� %   �� �     
"   
 �           X8    1� 6   � I   �%               o%   o           � M  4 
"   
 ��          �8    1� �   �� �     
"   
 ��          9    1� �   �� �     
"   
 ��          D9    1� �   �� �     
"   
 ��          �9    1� �   �� �  	   
"   
 ��          �9    1� �   �� �  	   
"   
 ��          �9    1� �   �� �  	   
"   
 ��          4:    1� �   �� �     
"   
 �           p:    1� �   � �  	 �%               o%   o           � N    
"   
 �           �:    1� �   � �  	 �%               o%   o           � N    
"   
 �           X;    1�    � �  	 �%               o%   o           � N    
"   
 �           �;    1�     � �  	 �%               o%   o           � N    
"   
 �           @<    1� 5   � �   �%               o%   o           %               
"   
 �           �<    1� C   � �   �%               o%   o           o%   o           
"   
 �           8=    1� U   � �   �%               o%   o           %               
"   
 �           �=    1� e   � �   �%               o%   o           %               
"   
 �           0>    1� q   � �   �%               o%   o           o%   o           
"   
 �           �>    1� �   � �   �%               o%   o           %               
"   
 ��          (?    1� �   �� �  	   
"   
 �           d?    1� �   � �   �%               o%   o           %              
"   
 ��          �?    1� �   �� �  	   
"   
 ��          @    1� �   �� �  	   
"   
 ��          X@    1� �  
 �� �  	   
"   
 �           �@    1� �   � �  	 �%               o%   o           � 5   
"   
 �           A    1� �   � �  	 �%               o%   o           � N    
"   
    "    �%     start-super-proc ��%     adm2/smart.p \�P �L 
�H T   %              �     }        �GG %              
"   
   �       (B    6� 1     
"   
   
�        TB    8
"   
   �        tB    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �C    �� 1   � P   �        �C    �@    
� @  , 
�       �C    �� :   �p�               �L
�    %              � 8      �C    � $         � A          
�    � [   �
"   
 �p� @  , 
�       �D    �� �   �p�               �L"    , �   � .    � 0    ��     }        �A      |    "      � .    %              (<   \ (    |    �     }        �A� 2    �A"        "    �"      < "    �"    (    |    �     }        �A� 2    �A"    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �F    �� 1   � P   �        �F    �@    
� @  , 
�       �F    �� :   �p�               �L
�    %              � 8      �F    � $         � A          
�    � [   �
"   
 �p� @  , 
�       �G    �� >  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �H    �� 1   � P   �        �H    �@    
� @  , 
�       �H    �� :   �p�               �L
�    %              � 8      �H    � $         � A          
�    � [   �
"   
 �p� @  , 
�       �I    �� �   �p�               �L
"   
 , 
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
   (�  L ( l       �        tJ    �� 1   � P   �        �J    �@    
� @  , 
�       �J    �� :     p�               �L
�    %              � 8      �J    � $         � A          
�    � [     
"   
 �p� @  , 
�       �K    �� d  
 �p�               �L%     SmartWindow 
"   
   p� @  , 
�       L    �� {     p�               �L%      WINDOW  
"   
  p� @  , 
�       lL    �� 3    p�               �L%               
"   
  p� @  , 
�       �L    ��     p�               �L(        � N      � N      � N      �     }        �A
�H T   %              �     }        �GG %              
"   
  (   � 
"   
 �    �        �M    �� 1   �
"   
   � 8      �M    � $         � A          
�    � [   �
"   
   �        PN    �
"   
   �       pN    /
"   
   
"   
   �       �N    6� 1     
"   
   
�        �N    8
"   
   �        �N    �
"   
   �       O    �
"   
   p�    � [    
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �O    �A"    �A
"   
   
�        P    �@ � 
"   
 "      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p N�    � �      
�    �     }        �%               %      Server  - �     }        �    "    � N    �%                   "    � N    �%      NONE    p�,  8         $     "            � �    �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        XR    �� 1   � P   �        dR    �@    
� @  , 
�       pR    �� :   �p�               �L
�    %              � 8      |R    � $         � A          
�    � [   �
"   
 �p� @  , 
�       �S    �� �   �p�               �L"    , p�,  8         $     "            � !   �
�     "    �%     start-super-proc ��%     adm2/visual.p ��   � ,     � (!     � *!  "   
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �T    �� 1   � P   �        �T    �@    
� @  , 
�        U    �� :   �p�               �L
�    %              � 8      U    � $         � A          
�    � [   �
"   
 �p� @  , 
�       V    �� i   �p�               �L"    , � 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP \�%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %      initializeDataObjects 0 0   A    �    � �!   
�    � �!   �A    �    � �!     
�    � �!   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %     buildDataRequest ent0 A    �    � �!   �
�    � �!   %     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
"    
 �
"   
 �%     contextHelp 
"    
   
�    
�    %               
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 (�  L ( l       �        hZ    �� 1   � P   �        tZ    �@    
� @  , 
�       �Z    �� :   �p�               �L
�    %              � 8      �Z    � $         � A   �     
�    � [   �
"   
 �p� @  , 
�       �[    �� ~   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        H\    �� 1   � P   �        T\    �@    
� @  , 
�       `\    �� :   �p�               �L
�    %              � 8      l\    � $         � A   �     
�    � [   �
"   
 �p� @  , 
�       |]    �� 5   �p�               �L%              (        �     }        �G� �   �G� 
"   
 �
"   
   �        ^    �%              
"   
 �
"   
 ��     }        �%               
"   
 �%      CLOSE   %               %     Carga-Temporal  %      Generar-Transferencia �� 
"   
 �
"   
 
"   
 ��         _    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � "  	   %               
"   
 �
�    %     createObjects    �     }        �%     initializeObject � �     }        �"      � N"     � \"     %              � b"         "    %               %               � �"     "      0a                              Ta        <a  @a  Da          � �"  	   Ha         "      � �"     �  �`  �`        "      �a                              �a        �a  �a  �a          � �"     �a         %              � X     �  ta  �a        %                   "      %                   � �"     z          "    �"      �b        �b  �b  �b          �b                              �b         "      � �"     � �"     �  xb  �b           (       "    � #    �    "    %                   � �"     z          "    �"      �c        �c  �c  �c          �c                              �c         "      � �"     � �"     �  �c  �c         "     �"    �&    &    &    &        %              %               *          "      � #   ��     }        �    "      � #   �"     �"    �&    &    &    &        %              %               *     � #      "      "       "           � 6#     z          "    �"      f        f  f  f          (f                              f         "      � �"     � �"     �  �e  �e              "      � 8#     �     }        �� ?#  !   "      %      
       "     t"    t&    &    &    &        %              %               * 	   � a#   t"    t� j#   t     � x#     z          "    �"      �g        �g  �g  �g          �g                              �g         "      � �"     � �"     �  tg  �g         ! "      �     }        �� z#  #   "      "    �lh                             � �#   �p�  @h  Lh         "      "      "      "      "      &    &    &    &        %              %               *     "      "      &    &    &    &        %              %               * 	    *    � �#     %      
       � �#  0   %               "      "      "      "       "       &    &    &    &            "      &        "      &    "      "      �             %               �             %              (        �     }        �G� �   �G� 
"   
 �
"   
   �     }        �
�    
"   
 �"    ^
"   
 ^"      "      "      "       "       &    &    &    &            "      &        "      &    "      "      
"   
   %      CLOSE   %               %      Valida      �  � $  	 �� $  	   � $  	   � $  	   %      Transferencia-Salida \�    �  � $  	 �� $  	   %      Transferencia-Ingreso �    �  � $  	 �� $  	   * !   *     * "   * #   "      "      "      "       "       &    &    &    &            "      &        "      &    "      "      �             %              �             %               %      SUPER   " $     � y$     "       &    &     4   %              $    4  �      %              &    � 4                      "       � |$     "       � $  	  � $  	  � �$   �%              "       "      &    &    &    &    &    &    &    &    L    0        %              %              %              %              �     }        �� �$  >   %      
       � �$     "      � $  	   "     \"    \&    &    &    &        %              %              "       " &          " &     %              " &     " &     " &     " &     %               � #      +      C  � �$     "       "                 "      � �$         "      � 8#     "      "      "      "      "      "      &    &    &    &    &    &    &    &    &    &    &    &    �    h    L    0        %              %              %              %              %              %              " '     " '     " '     " '     " '     " '     " '     " '     " '     "      "      "      "      "      "      " '     � #    ��" '     � "   %     ALM\ALMACSTK    " %         �  � $  	 �� $  	   %     alm/almacpr1    " %     � �$         �  � $  	 �� $  	   �  %   �%              %              %              %              %              " '     " '     $    " '     &    &    $    " '     &    &    &    &    &    &    &    &    &    &    & 	   & 	   & 
   & 
   �    h    L    0        %              %              %              %              %              %              �     }        �� $  	       "      � %     � %  "   %      
       � '%     %      
       � @%  +   � $  	   � l%         C  � �$         " '     � 8#     "     �"    �&    &    &    &        %              %              " )   t(        "      " )     %               %              � $  	  � $  	      " *     %              "     t"    t&    &    &    &        %              %               *     � �%  -   � $  	   "            "       %              "       "      "      �  %     %              � %     " (     " (     +      C  � �%         C  � �%     "       "       %              " '     " '     " '     " '     " '     " '     " '     " '     " ' )    " '     "      "      " 	     %              %               %               " '     � #    ��" '     " (     � "        " (     %              %     alm/almdcstk    " (         �  � $  	 �� $  	   %     alm/almacpr1    " (     � �$         �  � $  	 �� $  	   8    "    � �%   �� �%     � $  	   T   %              "      � |$   "     t"    t"    t&    &    &    &    &    &    0        %              %              %               * #   � &   t"    t� "&   t"    t� $  	   "     t"    t"    t&    &    &    &    &    &    0        %              %              %               * #   � 9&   t"    t� $  	   %$     vtagn/stock-comprometido \�" #   �" #   �" +   �     "          " #     " +     � W&   t    " #     " +     %      
       � u&     "      %      
       � �&     "      � $  	   � �&                     �           �   l       ��                 f  �  �               �                    O   ����    e�          O   ����    R�          O   ����    ��        $  u  �   ���                       `M     
                    � ߱              v  (  �      �M      4   �����M                �                      ��                  w  �                  �                       w  8  �  �  x  N            z  �  `      \N      4   ����\N                p                      ��                  {  �                  ��                       {  �  �  o   |      ,                                 �  �   }  |N      �  �   ~  �N      $  $    �  ���                       �N     
                    � ߱        8  �   �  �N      L  �   �  O      `  �   �  4O          $   �  �  ���                       dO  @         PO              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               з                    O   ����    e�          O   ����    R�          O   ����    ��      |                       �          �  $  �    ���                       �O     
                    � ߱                  �  �                      ��                   �  �                  T�                     �  4      4   �����O      $  �  �  ���                       $P     
                    � ߱        �    �  4  D      8P      4   ����8P      /  �  p                               3   ����LP  �  �   �  XP          O   �  ��  ��  �P                               , �                          
                               �      ��                            ����                                                        �   l       ��                  x    �                �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��`               �  	  �               P�                    O   ����    e�          O   ����    R�          O   ����    ��      �   r   �                     �`  |`  �   X`    d`  p`  8    �           �`      4   �����`      O   �  ��  ��  �`  H  �   �     |  o   �           �`                          �  $  �  �  ���                       `a                         � ߱        ,  $  �     ���                       �a                         � ߱        �  $  �  X  ���                       b                         � ߱                      H  p               ��                  �  �  0              h�                �     �  �      O   ����  e�          O   ����  R�          O   ����  ��      �  $  �  t  ���                        b                         � ߱        �  $  �  �  ���                       Hb                         � ߱        P  $  �  $  ���                       �b                         � ߱        �    �  l  |      c      4   ����c      O   �  �� ��      �  $  �  �  ���                       Xc                         � ߱        D  $  �    ���                       d                         � ߱          A  �         �   ��         �  Pd                                        d   $d                   �  �           0d  @d           8d  Hd         �            �   �    �    �  (  �      �d      4   �����d                �                      ��                  �  �                  �                       �  8  �d                         � ߱          $ �  �  ���                           �  (  �      �d      4   �����d                �                      ��                  �  �                  ��                       �  8  �d                         � ߱            $  �  �  ���                           A  �         p   ��         \  e                                        �d   �d                   �  �           �d  e           e  e         �            �   �    �	    �  �  l	      Le      4   ����Le                |	                      ��                  �  �                  ��                       �   	  �	  	  �  �	                                    �	  3   ����Xe      3   ����de      O   �  �� ��      @
  9   �     pe                     |e                         � ߱        l
  $  �  �	  ���                       �
  $  �  �
  ���                       �e                         � ߱        H  $  �  �
  ���                       4f                         � ߱        Hf                         � ߱        t  $ �    ���                       �    �  �        lf      4   ����lf                                      ��                  �  �                  |�                       �  �  �  	  �  P                                    `  3   �����f  p  3   �����f      3   �����f      O   �  �� ��      `  A  �       	 �   ��         �  �f                                        �f   �f                   L  @           �f  �f           �f  �f         �               ,    �    �  |  �      g      4   ����g                                      ��                  �  �                  4�                       �  �  l  	  �  <                                    L  3   ���� g  \  3   ����,g      3   ����8g      O   �  �� ��      �  $  �  �  ���                       Dg                         � ߱        `  $  �    ���                       �g                         � ߱        h                        � ߱        �  $ �  4  ���                             �  �  $      h      4   ����h                4                      ��                  �  �                  ��                       �  �  �  	  �  h                                    x  3   ����(h      3   ����4h      O   �  �� ��      �  �   �  xh      �  �   �  �h  �  �   �  �h  P  �   �  �h        `      �          �  �      ��             
     �  �  �              ��                4     �  �      �  �       ��                            7   ����         ��                     �            ,                  6   �       P   ��                    �            ,                                                                �  �                                   @            l   |        O   ����  e�          O   ����  R�          O   ����  ��      �  A  �         X   ��         D  �h                                        �h   �h                   �  �           �h  �h           �h  �h         �            t   �    �    �  �  T      i      4   ����i  	              d                      ��             	     �  �                  �                        �  �  |  :   �                 O   �  �� ��      \  A  �       	 �   ��         �  \i                                        $i   0i                   H  <           <i  Li           Di  Ti         �               (          �  x  �      �i      4   �����i  
                                    ��             
     �  �                  �                       �  �    :   �                 O   �  �� ��      �  A  �        �   ��                                                                 �  �                                   @            �   �        �  �  x      �i      4   �����i                �                      ��                  �                    �                       �    �  	  �  �                                    �  3   �����i  �  3   �����i      3   �����i      O      ��  ��  �i  h  s     0                           �  \  �       ��                            7   ����           ��                     �            �                  6                ��                    �            �                                                                h  \                                   @            <   L          �i  �i  �i              �         ��                            7   ����          ��               @j   �            d                  6           �   ��         �  @j   �            d                                                        j   j                   �  �            j  0j           (j  8j         �            �   �          �j  �j          |     �j  @         �j          �j  @         �j              � ߱            $        ���                                     �                                               �          t  �    �             ���                                                                                                       	     0   @   P   `   p   �      	     0   @   P   `   p   �              ��                             ��                             ��                              ��        �                  ����                            h  	  �         =   �           �                         f/                    �           �   l       ��                      �               p                    O   ����    e�          O   ����    R�          O   ����    ��             �   �       �j      4   �����j      n        �          (k            ,      4k      4   ����4k      �     Hk    ��                            ����                                            �           �   l       ��                  "  2  �               h                    O   ����    e�          O   ����    R�          O   ����    ��      \k  �               � ߱        @  Z   ,  �    �        Pk                  �               �              �              � ߱        l  h   .      �        hk              �  s   0  �                 �             �         ��                            7   ����           ��                     �            d                  6   0         �   ��                    �            d                                                                �  �                                   @            �   �          tk  �k  �k              ,  |       ��                            7   ����          ��               �k   �            �                  6   0           ��         �  �k   �            �                                                        �k   �k                   T  H           �k  �k           �k  �k         �                4          l  l          �  h      
   1  �� �             (l    ��                              ��        �                  ����                            �                         f/                    �           �   l       ��                  8  B  �               �                    O   ����    e�          O   ����    R�          O   ����    ��      �     ?  4l  }          O   @  ��  ��  Hl    ��                            ����                                            �           �   l       ��                 H  h  �               �                    O   ����    e�          O   ����    R�          O   ����    ��      �   /   O  �                                  3   ����\l  �    P          pl      4   ����pl      O   P  ��  ��  �l                �              �      ��                S  Y  �              �                �     S  4      O   S     ��  �l      O   S     ��  �l    /   T                                   3   �����l  `    U  8  H      �l      4   �����l      O   U     ��  �l  �  /   W  �                                 3   �����l        X  �  �      m      4   ����m      O   X     ��  0m      Z  �        <m      4   ����<m      8  Z  !   X    [  8  H      Dm      4   ����Dm      8  [      �    \  t  �      Lm      4   ����Lm      8  \  "   <    ]  �  �      Tm      4   ����Tm      8  ]  #         L      �          �  �      ��                  _  a  �                              �     _  �      x  �       ��                            7   ����         ��                     �                              6   _       <   ��                    �                                                                            �  x                                   @            X   h        O   ����  e�          O   ����  R�          O   ����  ��          :   `             \	  s   b  $                 	          �  P  �       ��                            7   ����           ��                     �            �                  6   b            ��                    �            �                                                                \  P                                   @            0   @          \m  hm  tm              �         ��                            7   ����          ��               �m   �            X                  6   b        �   ��         |  �m   �            X                                                        �m   �m                   �  �           �m  �m           �m  �m         �            �   �          �m  n          p  �  $n  @         n          Ln  @         8n              � ߱            $   c  	  ���                         ��                             ��                              ��        �                  ����                            �                         f/                    �           �   l       ��                 n  �  �               X                    O   ����    e�          O   ����    R�          O   ����    ��      r$   $                   �          �  /   y                                3   ����`n            8                      3   ����tn                0                      ��                  |  �                  �                       |  H        @      �          �  �      ��                  }  �  �              0                       }  �      l  �       ��                            7   ����           ��          	     �n    �                              6   }         D   ��        	 0  �n    �                                                                    �n   �n                   �  �           �n           �n                      `   p        O   ����  e�          O   ����  R�          O   ����  ��          �     �n                 $  P          @  H    0                                        $     ��                              ��        �                   ��                            ����                                                       �   l       ���               �  �  �               (&                    O   ����    e�          O   ����    R�          O   ����    ��            0      �  �      �  |      ��                 �  �  �              �&                       �  �       \  �       ��                            7   ����        ��          
           �            �                  6   �           ��         
           �            �                                                                h  \                                   @            <   L        O   �     ��  @o      O   �     ��  Lo      O   ����  R�      �  A  �      & 4   ��           �o                                        Xo   do   xo   �o                   �  �           �o  �o  �o  �o           �o  �o  �o  �o         �            P   l    �    �  �  @      8p      4   ����8p                P                      ��                  �  �                  p0                       �  �  �  	  �  �                                    �  3   ����Lp  �  3   ����Xp  �  3   ����lp      3   ����xp      O   �     ��  �p  �  A   �        @   ��         ,  �p                                        �p   �p                   �  �           �p  �p           �p  �p         �            \   p    L  9   �  '   �p      '               q      '               q      &               8q      '               Dq      '               Pq      '               \q      '               hq      '               |q      '               �q      '               �q      '               �q      ' !       !       �q      '               �q      '                   � ߱        �  V   �  �  ���                              �      �	  �      �	  �	      ��                 �  �  �	              43                �     �  x         p       ��                            7   ����        ��               �r    �            �                  6   �       	   ��         �  �r    �            �                                                        r   r   r   (r   4r   @r                   �	  t	           Lr  \r  lr  |r  �r  �r           Tr  dr  tr  �r  �r  �r                      ,	   P	        O   ����  e�          O   ����  R�          O   ����  ��        9   �  "   Ls      "               Xs      "               ds      "               ps      "               |s      "               �s      "               �s      "               �s      "               �s     " 
       
       �s      "               �s     	"               �s      "               �s     "               �s     "               �s     "                t      "               t      "               t      " -       -       $t      %                   � ߱        <  V   �  �	  ���                        �  /   �  h     x                          3   ����,t            �                      3   ����Ht  �    �  �  �      Tt      4   ����Tt      O   �     ��  lt  �  /   �       (                          3   ����xt  X        H                      3   �����t            x                      3   �����t        �  �  �      �t      4   �����t      O   �     ��  �t  �  A  �       `   ��  
         v                                        �t   �t   �t   u   u   ,u   @u   Lu  	 Xu  
 �u                   �  �           �u  �u  �u  �u  �u  �u           �u  �u  �u  �u  �u   v         �            |   �    (    �           �v      4   �����v      O   �     ��  �v  �    �  D  �      �v      4   �����v                �                      ��                  �  �                  $<                       �  T  T  	  �                                        3   �����v  $  3   �����v  4  3   ����w  D  3   ����w      3   ����(w      O   �     ��  4w  @w                     Lw                     dw       	       	           � ߱            V   �  l  ���                                   %  P          @  H    0                                    �  %     ��                             ��                             ��                            ����                                     =   �  "       =   �  '            &                  �           �   l       ��<               �  @  �               =                    O   ����    e�          O   ����    R�          O   ����    ��      |  A   �           ��           �w                                        �w   �w                   h  \           �w  �w           �w  �w         �            4   H    �  �   �       �   �                 d  �      L    l  ��          �w  �  <  4              �G                       �  �      D  �  �        d                      7   ����        )                      �            �                  6   �          )                     �            �                                                                P  D                                   @            $   4         )     8   �  )         �  L  �         �                                                                                                                                                                                )                                                                                       J   �               ��                                                           �w                      �              O   �     ��  Hx      O   �     ��  Tx      O   ����  R�          �  �  �      `x      4   ����`x                                      ��                  �                    @N                       �  �  �  A          p   ��         \  �x                                        �x   �x                   �  �           �x  �x           �x  �x         �            �   �           �  l      �x      4   �����x                |                      ��                                      DO                            �  	    �                                        3   �����x      O        ��  y  y      (                y                          � ߱        L  V     �  ���                        �	  9     '   Hy      '               Ty      '               `y      '               ly      '               xy      '               �y      '               �y      '               �y      '               �y      '               �y      '               �y      ' )       )       �y      ' !       !       �y      '               �y      (                   � ߱         
  V     \  ���                        0
  9            �     '      P
  :          ; ; ;              5 5 5      % % %      $ $ $                                                      
 
 
                              : : :      7 7 7                                                                              8 8 8      ) ) )      & & &      " " "      # # #      * * *      + + +      , , ,      - - -      . . .      / / /      0 0 0      1 1 1      2 2 2      3 3 3      4 4 4      6 6 6      ' ' '      ! ! !                                         	 	 	      ( ( (                                              9 9 9           �  9     "   z      "               z      "               (z      "               4z      "               @z      "               Lz      "               Xz      "               dz      "               pz      " -       -       |z     " 
       
       �z      "               �z     	"               �z      "               �z     "               �z     "               �z     "               �z      "               �z      "                {      " -       -       {      " 	       	       {      (                {      (                   � ߱        �  V       ���                        �  9   6     ,  �   7  "      �  0                                                  # # #                      ) ) )                                             $ $ $                      , , ,      - - -      % % %                      ! ! !      " " "                              * * *              	 	 	              ( ( (             + + +                 & & &      ' ' '                      . . .              / / /              
 
 
              0 0 0              1 1 1   �  /   8  X     h                          3   ����H{            �                      3   ����d{  �    9  �  �      p{      4   ����p{      O   9     ��  �{  x  /   :                                 3   �����{  H        8                      3   �����{            h                      3   �����{        ;  �  �      �{      4   �����{      O   ;     ��  �{              *  $                                             (  �          h  |   T (           ��                                                              $   4   D          $   4   D      �   ( *   ��                             ��                            ����                              =   <  "     =   <  '   ,  =   <         =   <                            �           �   l       ��|               F  r  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      �    O  �   L      �{      4   �����{                \                      ��                  O  R                  4�                       O  �   �  	  P  �                                        3   ����|      O   Q  ��  ��  |  |  $  S  �  ���                       $|                         � ߱              �                 �  �      ��                  U  o                ��                �
     U        �         ��                            7   ����         ��                     �            X                  6   U       |   ��                    �            X                                                                �  �                                   @            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �  A  V       # �   ��         p  �|                                        X|   d|   p|                 �  �           ||  �|  �|           �|  �|  �|         �            �   �    (    Z    �      �|      4   �����|                �                      ��                  Z  ]                  ��                       Z       	  [  �                                    �  3   ����}  �  3   ����}     3   ����}      3   ����(}      O   \  ��  ��  4}  �  A  ^       # �   ��         x  �}                                        @}   L}   X}                 �  �           d}  t}  �}           l}  |}  �}         �            �   �        b    �      �}      4   �����}                �                      ��                  b  e                  �                       b  (  �  	  c  �                                    �  3   �����}      3   �����}      O   d  ��  ��  ~  4	  /   f  <     L                          3   ����~  |        l                      3   ����8~  �        �                      3   ����D~            �  �                  3   ����P~      $   f  	  ���                                +                   � ߱              g  P	  �	      \~      4   ����\~                �	                      ��                  h  n                  ��                       h  `	  �
  	  i  
                                     
  3   �����~  0
  3   �����~  @
  3   �����~  P
  3   �����~  `
  3   �����~  p
  3   �����~  �
  3   �����~      3   ����      O   m  ��  ��        O   p  ��  ��                +                  �
                                       +     ��                             ��                            ����                                #      �%�          �  �
   �X                              
 �                                                                    ,      �         ('                                     
 �                                                                   �      �         a#                                    
 �                                                                �  C'    �  -     o7'                                    
 �                                                                �  W'    �         J'                                     
 �                                                                   A      �       ^'                                      �                                                                                                                                       �   d d     �   �� (�!(  � �                                               �                                                                         d     D                                                                 P   4 3d                                                           g'  G     p  4 Xl                                                         \     �                      m   \  � �p                                 J                 y'                @      \  �	 �
p                                 S                 �'                @      H  ���%�                                 �          �            D                                                                    TXS appSrvUtils CMOV Almcmov Detalle CodCia CodAlm UndVta CodUbi StkAct StkMin StkMax StkRep StkIni VInMn1 VInMn2 VCtMn1 VCtMn2 FchIng FchSal FchInv SelInv FacEqu desmat codmat AlmDes CodMar CodAnt StkActCbd Libre_c01 Libre_c02 Libre_c03 Libre_c04 Libre_c05 Libre_d01 Libre_d02 Libre_f01 Libre_f02 StkComprometido StockMax StockSeg StockMaxSeg ITEM TipMov CodMov NroDoc FchDoc CodMon NroItm TpoCmb CanDes Factor PreLis PreUni PreCos CodUnd ImpCto Ajuste AlmOri ImpMn1 ImpMn2 VctoMn1 VctoMn2 StkSub CodAjt NroSer PorDto ImpDto ImpLin AftIgv AftIsc Dsctos IgvMat PreBas ImpIgv ImpIsc CanDev Pesmat NroAnt Por_Dsctos Flg_Factor HraDoc StkSubCbd VctoMn1Cbd VctoMn2Cbd T-CMOV FchAnu NroRf1 NroRf2 CodPro CodCli CodTra CodVen Observ TotItm usuario FlgSit HorSal HorRcp CodDoc CodRef NroRef FlgEst FlgCbd FchCbd FlgFac NroFac NomRef cco Area ModAdq NroRf3 Libre_l01 Libre_l02 AlmFinal LPN DateUpdate HourUpdate UserUpdate CrossDocking AlmacenXD ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST s-codcia s-user-id s-AlmDes wWin BUTTON-1 BUTTON-2 COMBO-BOX-AlmDes Seleccione un almac�n Almmmatg Cat�logo de Materiales BROWSE-3 x(3) X(6) X(45) X(4) ZZZ,ZZZ,ZZ9.99 fMain X(256) GUI CARGA AUTOMATICA DE LOS ALMACENES DE REMATE DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   COMBO-BOX-AlmDes BUTTON-1 BROWSE-3 CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE iStartPage ADM-ERROR ADM-CREATE-OBJECTS x-Cab x-Linea Rpta DiasTrabajados Excel (*.xls) *.xls Carga del almac�n de Remate chExcelApplication chWorkbook chWorksheet cRange iCountLine iTotalColumn cValue Excel.Application Workbooks OPEN Sheets A Range VALUE  Almacen 99 x(3) Valor del almac�n no registrado: B 999999 Valor del producto no reconocido: Producto no registrado C Valor de la cantidad no reconocido: QUIT Error en el archivo EXCEL Haga una copia del archivo y vuelva a intentarlo CARGA-TEMPORAL DISABLE_UI ENABLE_UI EXITOBJECT ADM-ERROR FacCorre Correlativos por documento Almdmov Almmmate GENERAR-TRANSFERENCIA plDeep Si  -  INITIALIZEDATAOBJECTS r-Rowid Almtdocm I No se pudo bloquear el correlativo de ingreso por tansferencia Almac�n: HH:MM:SS 999 U S T NO se puede hacer la transferencia La guia ha sido alterada Revisar el documento original en el sistema R TRANSFERENCIA-INGRESO x-NroDoc s-NroSer x-Item NO se pudo bloquer el correlativo por almacen HH:MM TRANSFERENCIA-SALIDA pComprometido Selecc Seleccione un almac�n de Remate C�digo NO asignado al almac�n C�digo NO asignado al almac�n NO se puede transferir mas de para el producto del almac�n OK VALIDA mate01 mate02 mate03 Almd01 almd02 almd03 almd04 almd05 almd06 Almd07 almc01 almc02 almc03 almc04 almc05 almc06 almc07 almc08 almc09 almc10 Almac�n!Origen Descripci�n DesMat Unidad!Stock UndStk Cantidad Almac�n de Remate IMPORTAR EXCEL GENERAR TRANSFERENCIA alm01 Matg01 doc01 �  d1  �  8      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
 pcProcName  �   ��      �         pcProcName      ��              
 pcProcName      ��      $        piPageNum       ��      H        piPageNum       ��      l        pcPageList      ��      �        pcProc  �  ��      �        pcLinkName      ��      �        pcLinkName    ��      �       
 phTarget        ��              phTarget        ��      @        piPageNum       ��      d        pcValue     ��      �        piPageNum       ��      �        pcAction        ��      �       
 phAppService        ��      �        pcMode     ��             
 phSource    D  ��      8        phSource        ��      \       
 phSource    �  ��      �        pcText  �  ��      �        pcText      ��      �        pcText  �  ��      �       
 phObject      ��             
 phObject        ��      (        phObject        ��      L        pcField     ��      l        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller    �  ��      �        phCaller        ��      �        phCaller    (  ��               pcMod   H  ��      @        pcMod       ��      `       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    �  ��      �        pdRow       ��              pdRow       ��      ,       
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   j	  �	  �	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props u  v  w  x  z  {  |  }  ~    �  �  �  �  �  �  �              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                     T	  �	     =                                   �  �  �	  �	     >                                         �	  ,
     ?                                       �	  d
     @                                       4
  �
     A                                   $  %  l
  �
     B                                   J  K  L  M  �
       C                                   S  T  �
  `     D               L                  adm-create-objects    �        x     x-Cab   �       �     x-Linea �        �     Rpta    �        �     DiasTrabajados         �     chExcelApplication  $            chWorkbook  D       8     chWorksheet `        X     cRange  �        t     iCountLine  �        �     iTotalColumn              �     cValue       H   E   d          �                  Carga-Temporal  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �           	  �  \     F               P                  disable_UI             �     G               �                  enable_UI   ,  .  0  1  2  l  �     H               �                  exitObject  ?  @  B  �  L     I               4                  Generar-Transferencia   O  P  S  T  U  W  X  Y  Z  [  \  ]  _  `  a  b  c  h      $      �        plDeep    �     J       �      �                  initializeDataObjects   y  |  }    �  �  �      %      ,     r-Rowid �  |     K             d                  Transferencia-Ingreso   �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    (      �     x-NroDoc    $  (           s-NroSer    @  (      8     x-Item      (      T     r-Rowid 4  �     L   �          �                  Transferencia-Salida    �  �  �  �  �                            6  7  8  9  :  ;  <  @      +            pComprometido   \  h     M             `                  Valida  O  P  Q  R  S  U  V  Z  [  \  ]  ^  b  c  d  e  f  g  h  i  m  n  o  p  r  0  %             �#                      4      %   Detalle �         �         �         �         �         �                                                        (         0         8         @         H         P         X         `         h         p         x         �         �         �         �         �         �         �         �         �         �         �                                     (         CodCia  CodAlm  codmat  UndVta  CodUbi  StkAct  StkMin  StkMax  StkRep  StkIni  VInMn1  VInMn2  VCtMn1  VCtMn2  FchIng  FchSal  FchInv  SelInv  FacEqu  desmat  AlmDes  CodMar  CodAnt  StkActCbd   Libre_c01   Libre_c02   Libre_c03   Libre_c04   Libre_c05   Libre_d01   Libre_d02   Libre_f01   Libre_f02   StkComprometido StockMax    StockSeg    StockMaxSeg $  D  L  0   ITEM    �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                             $         ,         4         <         D         L         T         \         d         l         t         |         �         �         �         �        �         �         �         �         �         �         �        �         �         �                                     CodCia  CodAlm  TipMov  CodMov  NroDoc  FchDoc  CodMon  NroItm  TpoCmb  codmat  CanDes  Factor  PreUni  CodUnd  ImpCto  Ajuste  AlmOri  ImpMn1  ImpMn2  VctoMn1 VctoMn2 StkSub  StkAct  CodAjt  NroSer  PorDto  ImpDto  ImpLin  AftIgv  AftIsc  PreBas  ImpIgv  ImpIsc  CanDev  Dsctos  IgvMat  PreCos  PreLis  Pesmat  CodAnt  NroAnt  Por_Dsctos  Flg_Factor  HraDoc  StkActCbd   StkSubCbd   VctoMn1Cbd  VctoMn2Cbd      4  <  :   T-CMOV  �         �                                             $         ,         4         <         D         L         T         \         d         l         t         |         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                                       (         0         <         H         T         `         l         x         �         �         �         �         �         �         �         �         �         �         �         CodCia  CodAlm  TipMov  CodMov  NroDoc  FchDoc  NroRf1  NroRf2  CodPro  CodCli  CodVen  Observ  TotItm  CodMon  TpoCmb  FlgEst  AlmDes  usuario NroSer  FlgSit  HorSal  HorRcp  FchAnu  CodDoc  CodRef  NroRef  CodTra  FlgCbd  FchCbd  FlgFac  NroFac  NomRef  ImpMn1  ImpMn2  cco Area    ImpIgv  ModAdq  NroRf3  HraDoc  Libre_c01   Libre_c02   Libre_c03   Libre_c04   Libre_c05   Libre_d01   Libre_d02   Libre_f01   Libre_f02   Libre_l01   Libre_l02   AlmFinal    LPN DateUpdate  HourUpdate  UserUpdate  CrossDocking    AlmacenXD   $            
   appSrvUtils D        8     s-codcia    d        X     s-user-id   �       x     s-AlmDes    �       �  
   wWin    �       �     COMBO-BOX-AlmDes    �  
 
     �  
   gshAstraAppserver               
   gshSessionManager   <         ,   
   gshRIManager    d         P   
   gshSecurityManager  �         x   
   gshProfileManager   �         �   
   gshRepositoryManager    �         �   
   gshTranslationManager   !        �   
   gshWebManager   ,!        !     gscSessionId    P!        @!     gsdSessionObj   t!        d!  
   gshFinManager   �!        �!  
   gshGenManager   �!        �!  
   gshAgnManager   �!        �!     gsdTempUniqueID  "        �!     gsdUserObj  ("        "     gsdRenderTypeObj    P"        <"     gsdSessionScopeObj  l"       d"  
   ghProp  �"       �"  
   ghADMProps  �"       �"  
   ghADMPropsBuf   �"       �"     glADMLoadFromRepos  �"    	   �"     glADMOk #    
   #  
   ghContainer 4#       (#     cObjectName P#       H#     iStart  p#       d#     cAppService �#       �#     cASDivision �#       �#     cServerOperatingMode    �#       �#     cFields          �#     iStartPage  $     C  $  CMOV    ($    \   $  Detalle @$    L  8$  ITEM    X$    L  P$  T-CMOV  t$  	 	    h$  Almmmatg    �$        �$  Almacen �$   !    �$  FacCorre    �$   "   �$  Almdmov �$   #    �$  Almmmate    �$   &    �$  Almtdocm          '   %  Almcmov          D   q  r  �  �  �  �  �  �  �  �  8  9  :  ;  R  ^  _  `  b  d  e  f  j  k  n  o  p  q  s  u  w  y  z  {  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  /	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  +
  6
  7
  9
  :
  ;
  <
  =
  >
  ?
  @
  A
  B
  C
  D
  E
  F
  G
  H
  I
  J
  K
  L
  M
  N
  O
  P
  Q
  R
  S
  T
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
                     	  
                  �  �  �  �  �  �  �  �  �  �  �  �      ;  W  Y  n  �      )  9  :  ;  >  ?  @  G  H  e  y  �  .  /  3  =  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �           u  �  �  �  �  �      "  A  B  H  R  W  ^  b  f  g  h  i  k  m      H� $ C:\Progress\OpenEdge\src\adm2\windowmn.i ()  f!  C:\Progress\OpenEdge\src\adm2\containr.i \)  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �)  ��  C:\Progress\OpenEdge\src\adm2\visual.i   �)  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  *  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    H*  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �*  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �*  Ds   C:\Progress\OpenEdge\gui\fn  �*  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i    +  Q.  C:\Progress\OpenEdge\gui\set `+  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �+  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �+  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i     ,  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  D,  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i x,  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �,  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i �,  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    ,-  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    p-  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �-  �j  C:\Progress\OpenEdge\gui\get �-  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    .  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    T.  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �.  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �.  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i  /  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   @/  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �/  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �/  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i   0  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  40  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i x0  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �0  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �0  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   ,1  2�   d:\newsie\on_in_co\APLIC\alm\w-remate-01.w       K  p      �1     5  $   �1  �   �      �1  �   �     �1     �     �1  �   �     �1     m     �1  �   e     2       #   2  �   �     ,2     �      <2  �   �     L2     �      \2  �   �     l2     �      |2  r   �     �2  n   �     �2     \  "   �2  i   W     �2     5     �2  P        �2  �        �2     �  !   �2  �   �     3     �     3  �   �     ,3     q     <3  �   o     L3     M     \3  g   3     l3          |3  O   �     �3  �   �     �3     �      �3  �   T     �3     �     �3  �   �     �3     �     �3  �   �     �3     �     4  �   �     4     �     ,4  �   �     <4     f     L4  �   U     \4     3     l4  �   0     |4          �4  }        �4     �     �4     d     �4          �4     �     �4  7   �     �4  �   �     �4  O   u     5     d     5          ,5  �   �
     <5  �   �
     L5  O   �
     \5     �
     l5     X
     |5  �   3
     �5  x   +
  
   �5  M   
     �5     
     �5     �	     �5  a   �	  
   �5  �  �	     �5     b	     �5  �  /	     6  O   !	     6     	     ,6     �     <6  �   �     L6     �     \6          l6  x        |6     �     �6     }     �6     y     �6     e     �6     L     �6  Q   <  
   �6     �     �6     �  
   �6     �     7     |  
   7  f   Q     ,7     �  	   <7  "   �     L7     �     \7     w     l7  Z   &     |7     .     �7     �     �7     �     �7     �     �7     �     �7  4   �       �7     M      �7     !       �7           