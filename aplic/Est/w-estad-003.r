	��V�r�Q@6  D �              �                                i� 36400117utf-8 MAIN C:\newsie\on_in_co\aplic\Est\w-estad-003.w,, PROCEDURE Resumen-por-vendedor,, PROCEDURE Resumen-por-vendcli,, PROCEDURE Resumen-por-resmat,, PROCEDURE Resumen-por-producto,, PROCEDURE Resumen-por-familia,, PROCEDURE Resumen-por-division,, PROCEDURE Resumen-por-climat,, PROCEDURE Resumen-por-cliente,, PROCEDURE Resumen-General,, PROCEDURE initializeObject,, PROCEDURE exitObject,, PROCEDURE Excel-2,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE Carga-Temporal,, PROCEDURE Carga-Lista-Clientes,, PROCEDURE Carga-Lista-Articulos,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER      �r              ��             1� �r  �             �D             8[    +   d� �  7   � `  8   d� �   R   X�   S   h� 4  T   �� �7  U   $1 |  V   �2 �  W   �6 �)  X   H` $  Y   la �  Z   �n �j  [   �� �H  \   �" �[  ]   |~ (  ^   �� p  _   � <%  `   P� #  a   T� �M  b   $9   c           0R �  ? �a i.  iSO8859-1                                                                           �p   B �                4q                 �                  �                   �3     �3  
 ��    �  <q         � �   �r      �r          �                                             PROGRESS                         �           
    
                    �              �                                                                                                     
                                                                                                         �             �             �                                                                                          �  ��                      �                                                                                          �  ��         �             x                                                                                          �             �             �                                                                                          �               �            0  �            D  �            X  �            l  �                �            �         �       ^  L  ,=  J   x=  ^  ,�      �=         ^             �9          t:      �   P         �       �  L  0T  L   |T  �  Y�      �T  1       �             �=          �A      �     	       �       ^  L  �k  L   $l  ^  Y�      Pl  1       ^             lU          TY      �                estavtas                         PROGRESS                         H     �  �      �                         �	P            �  ��                              �  �                      �  �  N      TABLACODIGOCAMPO-LOGICALCAMPO-CHARACTERCAMPO-DATECAMPO-DECIMALFECHAHORAUSUARIO                                                             "       	          �         �       �  L  8n     Ln  �  &_      xn         �             m          Pm      �            �       �  L  �o     �o  �  &_      �o         �             �n          �n      �   �         �       �  L  �p     �p  �  ��      �p         �             �o          p      �   ,	        �  
    
                  �  \	             	                                                                                                    
  �	  !      T	  
    
                  @	  
             �	                                                                                          !          
  �
  3       
  
    
                  �	  �
             p
                                                                                          3          
  0  @      �
  
    
                  �
  `                                                                                                       @          
  �  S      X  
    
                  D               �                                                                                          S          
  �  e        
    
                  �  �             t                                                                                          e          
  4  z      �  
    
                  �  d                                                                                                        z          
  �  �      \  
    
                  H               �                                                                                          �          
  �  �                               �  �             x                                                                                          �            8  �      �                        �  h             $                                                                                          �            �  �      `  
    
                  L               �                                                                                          �          
  �  �        
    
                  �  �             |                                                                                          �          
  <  �      �  
    
                  �  l             (                                                                                          �          
  �  �      d                        P               �                                                                                          �            �  �                              �  �             �                                                                                          �            @  �      �                        �  p             ,                                                                                          �                      h                        T  �             �                                                                                                      �  %   �&  �      �&                         LJP            �&  
�                              �  l                      �  |        CODFAMSUBFAMNOMSUBFAM                                 P  &   �&  �      �&                         n	�O            �&  Jg                              �  8                      �  H  _      CODCLINOMCLICANALNOMCANALCODPAISCODDEPTCODPROVCODDISTGIRCLINOMGIRCLINROCARDCLFCLICODUNICOFLGSIT                                                                       	          
                                                              '   .'  �      .'                         �.hQ            :'  NI                              �  �                      P  �  n      CODMATDESMATUNDSTKCODPROCODFAMSUBFAMDESMARLICENCIACODASOCSUBTIPOCANEMPMONVTACTOLISCTOTOTTPOCMBRANKINGCATEGORIA                                                                       	                                                                                                    �  (   `'  �      `'                         x�O            m'  ��                              �  �                      �  �        CODPRONOMPRO                    �  )   �'  �      �'                         8y�O            �'  ��                              �  P                      l  `        CODVENNOMVEN                    P  -   m(  �      m(                         (.=P            |(  1�                              �                        ,          APLIC-IDUSER-IDCODFAM                                   /   4*  �      4*                         �P            4*  (�                              �  �                      �  �        USER-IDCODDIVAPLIC-ID                                   0   A*  �      A*                         �x�O            M*  �;                              �  �                      �  �  ,      CODDIVDESDIVLOCALIDADCANALVENTANOMCANALVENTA                                                  �  1   b*  �      b*                         LJP            k*  ��                              �  �                      �  �        CODFAMNOMFAM                    H  2   b*  �      b*                         LJP            k*  ��                             �  �                      �  4   �*  �      �*                         ��mQ            �*  ��                              �  �                      �  �  �      CODDIVCODCLICODVENCODMATIMPNACCIGVIMPNACSIGVIMPEXTCIGVIMPEXTSIGVCANTIDADCOSTONACCIGVCOSTONACSIGVCOSTOEXTCIGVCOSTOEXTSIGVPROMNACCIGVPROMNACSIGVPROMEXTCIGVPROMEXTSIGVTIPODATEKEYDIVDESDELIVERY                                                                         	          
                                                                                                                                  "  5   �*  �      �*                         ä�O            �*  ��                              �                         8!  $        DATEKEYFULLDATELABELDATEDESCRIPTIONCALENDARYEARCALENDARYEARLABELCALENDARHALFYEARLABELCALENDARHALFYEARCALENDARQUARTERCALENDARQUARTERLABELCALENDARWEEKCALENDARWEEKLABELCALENDARDAYOFWEEKCALENDARDAYOFWEEKLABELISWORKDAYISHOLIDAYISHOLIDAYNAMECALENDARMONTHCALENDARMONTHLABELCAMPANIA                                                                        	          
                                                                                                              �"  6   �*  �      �*                         �u�O            �*  L&                              �  �"                      �"  �"        NROCARDNOMNROCARD                       $  7   +  �      +                         pv�O            %+  �]                              �  X#                      �#  h#  ;      CODDEPTONOMDEPTOZONANOMZONACODPROVINOMPROVICODDISTRNOMDISTR                                                                       	          �$  8   D+  �      D+                         M�O            D+  �                              �  �$                      �$  �$        LICENCIADESCRIPCION                     �%  9   P+  �      P+                         �%�N            ]+  ��                              �  @%                      d%  P%        CODCIACODVENNOMVEN                                �'  :   w+  �      w+                         ��mQ            w+  ��                              �  &                      �&  &  �      CODDIVCODCLIIMPNACCIGVIMPNACSIGVIMPEXTCIGVIMPEXTSIGVCOSTONACCIGVCOSTONACSIGVCOSTOEXTCIGVCOSTOEXTSIGVPROMNACCIGVPROMNACSIGVPROMEXTCIGVPROMEXTSIGVTIPODATEKEYDIVDESDELIVERY                                                                         	          
                                                                                                    �)  ;   �+  �      �+                         R�mQ            �+  �4                              �  (                      �(  ,(  �      CODDIVCODCLICODFAMSUBFAMDESMARLICENCIAIMPNACCIGVIMPNACSIGVIMPEXTCIGVIMPEXTSIGVCOSTONACCIGVCOSTONACSIGVCOSTOEXTCIGVCOSTOEXTSIGVPROMNACCIGVPROMNACSIGVPROMEXTCIGVPROMEXTSIGVTIPODATEKEYDIVDESDELIVERY                                                                       	          
                                                                                                                                            �*  <   �+  �      �+                         p#�N            �+  ��                              �  x*                      �*  �*        CODCIALICENCIADESCRIPCION                                 �,  =   ,  �      ,                         %�mQ            ,  �                              �  H+                      ,  X+  �      CODDIVCODVENIMPNACCIGVIMPNACSIGVIMPEXTCIGVIMPEXTSIGVCOSTONACCIGVCOSTONACSIGVCOSTOEXTCIGVCOSTOEXTSIGVPROMNACCIGVPROMNACSIGVPROMEXTCIGVPROMEXTSIGVDATEKEYDIVDESTIPODELIVERY                                                                         	          
                                                                                                    /  ?   �,  �      �,                         �mQ            �,                                �  \-                       .  l-  �      CODDIVCODMATIMPNACCIGVIMPNACSIGVIMPEXTCIGVIMPEXTSIGVCANTIDADCOSTONACCIGVCOSTONACSIGVCOSTOEXTCIGVCOSTOEXTSIGVPROMNACCIGVPROMNACSIGVPROMEXTCIGVPROMEXTSIGVDATEKEYDIVDESTIPODELIVERY                                                                         	          
                                                                                                              t1  @   �,  �      �,                         �mQ            �,  ��                              �  �/                      `0  �/  �      CODDIVCODFAMSUBFAMDESMARLICENCIACODPROCODVENIMPNACCIGVIMPNACSIGVIMPEXTCIGVIMPEXTSIGVCOSTONACCIGVCOSTONACSIGVCOSTOEXTCIGVCOSTOEXTSIGVPROMNACCIGVPROMNACSIGVPROMEXTCIGVPROMEXTSIGVDATEKEYDIVDESTIPODELIVERY                                                                         	          
                                                                                                                                                          A   -  �      -                         �mQ            -  ��                              �  �1                      �2  2  �      CODDIVCODCLICODVENIMPNACCIGVIMPNACSIGVIMPEXTCIGVIMPEXTSIGVCOSTONACCIGVCOSTONACSIGVCOSTOEXTCIGVCOSTOEXTSIGVPROMNACCIGVPROMNACSIGVPROMEXTCIGVPROMEXTSIGVTIPODATEKEYDIVDESDELIVERY                                                                       	          
                                                                                                                           a T                                            
 R �         �7  �8  � � 4                                                                                                                                                                                                                                                                                                                                                          
                              Todos        Todos                                                                                                                                                                                                                                                                                          
             
             
                                         
                                                                                                               ? �       (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �     ? �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �                                                                                                                              	                                  <  <  <  <                             <  $<  0<  8<                              <<  H<  T<  `<                              d<  p<  |<  �<                              �<  �<  �<  �<                              �<  �<  �<  �<                              �<  �<  �<   =                              =  =  =  (=                                                                          Llave   x(8)    Llave       CanxMes ->>,>>9.99  CanxMes 0   VtaxMesMe   ->>,>>9.99  VtaxMesMe   0   VtaxMesMn   ->>,>>9.99  VtaxMesMn   0   CtoxMesMe   ->>,>>9.99  CtoxMesMe   0   CtoxMesMn   ->>,>>9.99  CtoxMesMn   0   ProxMesMe   ->>,>>9.99  ProxMesMe   0   ProxMesMn   ->>,>>9.99  ProxMesMn   0   �  ���	������ �     ��     ��     ��     ��     ��     ��     �      �-                �     i     	    j  p  x  �  �  �  �  �                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                                 DJ  PJ  XJ  dJ                              hJ  pJ  xJ  |J                              �J  �J  �J  �J                              �J  �J  �J  �J                              �J  �J  �J  �J                              �J  �J  �J  �J                               K  K  K  K                              K  (K  0K  <K                              @K  HK  PK  XK                              \K  dK  lK  tK                              xK  �K  �K  �K                              �K  �K  �K  �K                              �K  �K  �K  �K                              �K  �K  �K  �K                              �K   L  L  L                              L  $L  ,L  <L                              @L  LL  TL  `L                              dL  pL  xL  �L                              �L  �L  �L  �L                              �L  �L  �L  �L                              �L  �L  �L  �L                              �L   M  M  $M                              (M  8M  LM  dM                              hM  tM  �M  �M                              �M  �M  �M  �M                              �M  �M  N  N                              N  ,N  @N  XN                              \N  lN  �N  �N                              �N  �N  �N  �N                              �N  �N  �N  O                              O   O  4O  HO                              LO  `O  tO  �O                              �O  �O  �O  �O                              �O  �O   P  P                              P  0P  DP  \P                              `P  tP  �P  �P                              �P  �P  �P  �P                              �P  �P  Q  (Q                              ,Q  @Q  TQ  lQ                              pQ  �Q  �Q  �Q                              �Q  �Q  �Q  �Q                              �Q  R  R  ,R                              0R  DR  XR  lR                              pR  �R  �R  �R                              �R  �R  �R  S                              S   S  4S  LS                              PS  hS  |S  �S                              �S  �S  �S  �S                              �S  �S  T  ,T                                                                          Campania    x(20)   Campania        Periodo ZZZ9    A�o 0   NroMes  Z9  Mes 0   Division    x(60)   Division        CanalVenta  x(60)   Canal Venta     Producto    x(60)   Producto        Linea   x(60)   Linea       Sublinea    x(60)   Sublinea        Marca   x(20)   Marca       Unidad  x(10)   Unidad      Licencia    x(60)   Licencia        Proveedor   x(60)   Proveedor       Cliente x(60)   Cliente     Canal   x(30)   Canal       Tarjeta x(60)   Tarjeta     Departamento    x(20)   Departamento        Provincia   x(20)   Provincia       Distrito    x(20)   Distrito        Zona    x(20)   Zona        Clasificacion   x(20)   Clasificacion       Vendedor    x(60)   Vendedor        CanActual   ->>>,>>>,>>>,>>9.99 CANTIDAD-ACTUAL 0   CanAcumActual   ->>>,>>>,>>>,>>9.99 CANTIDAD-ACUM-ACTUAL    0   CanAnterior ->>>,>>>,>>>,>>9.99 CANTIDAD-ANTERIOR   0   CanAcumAnterior ->>>,>>>,>>>,>>9.99 CANTIDAD-ACUM-ANTERIOR  0   VtaSolesActual  ->>>,>>>,>>>,>>9.99 VENTA-SOLES-ACTUAL  0   VtaDolarActual  ->>>,>>>,>>>,>>9.99 VENTA-DOLARES-ACTUAL    0   CtoSolesActual  ->>>,>>>,>>>,>>9.99 CTO-SOLES-ACTUAL    0   CtoDolarActual  ->>>,>>>,>>>,>>9.99 CTO-DOLAR-ACTUAL    0   PromSolesActual ->>>,>>>,>>>,>>9.99 PROM-SOLES-ACTUAL   0   PromDolarActual ->>>,>>>,>>>,>>9.99 PROM-DOLAR-ACTUAL   0   VtaSolesAcumActual  ->>>,>>>,>>>,>>9.99 VENTA-SOLES-ACUM-ACTUAL 0   VtaDolarAcumActual  ->>>,>>>,>>>,>>9.99 VENTA-DOLARES-ACUM-ACTUAL   0   CtoSolesAcumActual  ->>>,>>>,>>>,>>9.99 CTO-SOLES-ACUM-ACTUAL   0   CtoDolarAcumActual  ->>>,>>>,>>>,>>9.99 CTO-DOLAR-ACUM-ACTUAL   0   PromSolesAcumActual ->>>,>>>,>>>,>>9.99 PROM-SOLES-ACUM-ACTUAL  0   PromDolarAcumActual ->>>,>>>,>>>,>>9.99 PROM-DOLAR-ACUM-ACTUAL  0   VtaSolesAnterior    ->>>,>>>,>>>,>>9.99 VENTA-SOLES-ANTERIOR    0   VtaDolarAnterior    ->>>,>>>,>>>,>>9.99 VENTA-DOLARES-ANTERIOR  0   CtoSolesAnterior    ->>>,>>>,>>>,>>9.99 CTO-SOLES-ANTERIOR  0   CtoDolarAnterior    ->>>,>>>,>>>,>>9.99 CTO-DOLAR-ANTERIOR  0   PromSolesAnterior   ->>>,>>>,>>>,>>9.99 PROM-SOLES-ANTERIOR 0   PromDolarAnterior   ->>>,>>>,>>>,>>9.99 PROM-DOLAR-ANTERIOR 0   VtaSolesAcumAnterior    ->>>,>>>,>>>,>>9.99 VENTA-SOLES-ACUM-ANTERIOR   0   VtaDolarAcumAnterior    ->>>,>>>,>>>,>>9.99 VENTA-DOLARES-ACUM-ANTERIOR 0   CtoSolesAcumAnterior    ->>>,>>>,>>>,>>9.99 CTO-SOLES-ACUM-ANTERIOR 0   CtoDolarAcumAnterior    ->>>,>>>,>>>,>>9.99 CTO-DOLAR-ACUM-ANTERIOR 0   PromSolesAcumAnterior   ->>>,>>>,>>>,>>9.99 PROM-SOLES-ACUM-ANTERIOR    0   PromDolarAcumAnterior   ->>>,>>>,>>>,>>9.99 PROM-DOLAR-ACUM-ANTERIOR    0   �   / ?�  ���3������                                                 �    �-                �     i    3 	    �  �  �  �  �  �  �  �              (  .  6  C  M  V  [  i  r  |  �  �  �  �  �  �  �  �      (  ;  N  b  v  �  �  �  �  �  �  �      2  H                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                                 �a  �a   b  b                              b  b   b  $b                              (b  0b  4b  8b                              <b  Hb  Pb  \b                              `b  lb  tb  �b                              �b  �b  �b  �b                              �b  �b  �b  �b                              �b  �b  �b  �b                              �b  �b  �b   c                              c  c  c  c                               c  ,c  4c  @c                              Dc  Pc  Xc  dc                              hc  pc  xc  �c                              �c  �c  �c  �c                              �c  �c  �c  �c                              �c  �c  �c  �c                              �c  �c  �c  d                              d  d   d  ,d                              0d  8d  @d  Hd                              Ld  \d  dd  td                              xd  �d  �d  �d                              �d  �d  �d  �d                              �d  �d  �d  e                              e  e  0e  De                              He  Xe  le  �e                              �e  �e  �e  �e                              �e  �e  �e   f                              f  f  (f  <f                              @f  Pf  df  xf                              |f  �f  �f  �f                              �f  �f  �f  �f                              �f  g  g  4g                              8g  Lg  `g  |g                              �g  �g  �g  �g                              �g  �g  �g  h                              h  h  0h  Hh                              Lh  `h  th  �h                              �h  �h  �h  �h                              �h  �h  �h  i                              i  ,i  @i  Ti                              Xi  li  �i  �i                              �i  �i  �i  �i                              �i  �i   j  j                              j  0j  Dj  `j                              dj  |j  �j  �j                              �j  �j  �j  �j                              �j  k  $k  <k                              @k  Xk  lk  �k                              �k  �k  �k  �k                                                                          Campania    x(20)   Campania        Periodo ZZZ9    A�o 0   NroMes  Z9  Mes 0   Division    x(60)   Division        CanalVenta  x(60)   Canal Venta     Producto    x(60)   Producto        Linea   x(60)   Linea       Sublinea    x(60)   Sublinea        Marca   x(20)   Marca       Unidad  x(10)   Unidad      Licencia    x(60)   Licencia        Proveedor   x(60)   Proveedor       Cliente x(60)   Cliente     Canal   x(30)   Canal       Tarjeta x(60)   Tarjeta     Departamento    x(20)   Departamento        Provincia   x(20)   Provincia       Distrito    x(20)   Distrito        Zona    x(20)   Zona        Clasificacion   x(20)   Clasificacion       Vendedor    x(60)   Vendedor        CanActual   ->>>,>>>,>>>,>>9.99 CANTIDAD-ACTUAL 0   CanAcumActual   ->>>,>>>,>>>,>>9.99 CANTIDAD-ACUM-ACTUAL    0   CanAnterior ->>>,>>>,>>>,>>9.99 CANTIDAD-ANTERIOR   0   CanAcumAnterior ->>>,>>>,>>>,>>9.99 CANTIDAD-ACUM-ANTERIOR  0   VtaSolesActual  ->>>,>>>,>>>,>>9.99 VENTA-SOLES-ACTUAL  0   VtaDolarActual  ->>>,>>>,>>>,>>9.99 VENTA-DOLARES-ACTUAL    0   CtoSolesActual  ->>>,>>>,>>>,>>9.99 CTO-SOLES-ACTUAL    0   CtoDolarActual  ->>>,>>>,>>>,>>9.99 CTO-DOLAR-ACTUAL    0   PromSolesActual ->>>,>>>,>>>,>>9.99 PROM-SOLES-ACTUAL   0   PromDolarActual ->>>,>>>,>>>,>>9.99 PROM-DOLAR-ACTUAL   0   VtaSolesAcumActual  ->>>,>>>,>>>,>>9.99 VENTA-SOLES-ACUM-ACTUAL 0   VtaDolarAcumActual  ->>>,>>>,>>>,>>9.99 VENTA-DOLARES-ACUM-ACTUAL   0   CtoSolesAcumActual  ->>>,>>>,>>>,>>9.99 CTO-SOLES-ACUM-ACTUAL   0   CtoDolarAcumActual  ->>>,>>>,>>>,>>9.99 CTO-DOLAR-ACUM-ACTUAL   0   PromSolesAcumActual ->>>,>>>,>>>,>>9.99 PROM-SOLES-ACUM-ACTUAL  0   PromDolarAcumActual ->>>,>>>,>>>,>>9.99 PROM-DOLAR-ACUM-ACTUAL  0   VtaSolesAnterior    ->>>,>>>,>>>,>>9.99 VENTA-SOLES-ANTERIOR    0   VtaDolarAnterior    ->>>,>>>,>>>,>>9.99 VENTA-DOLARES-ANTERIOR  0   CtoSolesAnterior    ->>>,>>>,>>>,>>9.99 CTO-SOLES-ANTERIOR  0   CtoDolarAnterior    ->>>,>>>,>>>,>>9.99 CTO-DOLAR-ANTERIOR  0   PromSolesAnterior   ->>>,>>>,>>>,>>9.99 PROM-SOLES-ANTERIOR 0   PromDolarAnterior   ->>>,>>>,>>>,>>9.99 PROM-DOLAR-ANTERIOR 0   VtaSolesAcumAnterior    ->>>,>>>,>>>,>>9.99 VENTA-SOLES-ACUM-ANTERIOR   0   VtaDolarAcumAnterior    ->>>,>>>,>>>,>>9.99 VENTA-DOLARES-ACUM-ANTERIOR 0   CtoSolesAcumAnterior    ->>>,>>>,>>>,>>9.99 CTO-SOLES-ACUM-ANTERIOR 0   CtoDolarAcumAnterior    ->>>,>>>,>>>,>>9.99 CTO-DOLAR-ACUM-ANTERIOR 0   PromSolesAcumAnterior   ->>>,>>>,>>>,>>9.99 PROM-SOLES-ACUM-ANTERIOR    0   PromDolarAcumAnterior   ->>>,>>>,>>>,>>9.99 PROM-DOLAR-ACUM-ANTERIOR    0   �   / ?�  ���3������                                                 �    �-                �     i    3 	    �  �  �  �  �  �  �  �              (  .  6  C  M  V  [  i  r  |  �  �  �  �  �  �  �  �      (  ;  N  b  v  �  �  �  �  �  �  �      2  H                                                          �m  �m  �m  �m  �m          �m            n  n  $n  4n  ,n                                                                     tt-codcli   x(11)   Codigo  Codigo      C�digo del Cliente  tt-nomcli   x(50)   NomCli  Cliente     �  ���������       �-                �     i     	    �  �                                                          @o  Lo  To  to  do                        xo  �o  �o  �o  �o                                                                     tt-codmat   X(6)    Codigo Articulo Codigo Articulo     tt-desmat   x(50)   DesMat  Descripcion     �  ���������       �-                �     i     	    �  �                                        lp  xp  �p  �p                                                                          tt-codigo   x(8)    tt-codigo       �  ��������� �     �-                �     i     	    �    ��                                               	          ����                            :      M.   �F    �-         S.  % !�    S.  & !�    S.  ' aR    S.  (     S.  ) e�    �-         �-         �-         M.  - �S    �-         [.  / �    S.  0 )d    S.  1 ��    a.  4 )!    S.  5 �&    S.  6 Pg    S.  7 +�    S.  8 �    a.  : $    a.  ; u2    �-  < �M    a.  = ��    �-  	       a.  ? L    a.  @ ��    a.  A 9    undefined                                                               �        �   l      �                 �����               l��                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     7          assignFocusedWidget         �      �            LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    $       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    6       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          L       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    X       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    d       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    w       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    #      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    7      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    E      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    U      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    f      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    s      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    8  A  m        �
   ��         �
  �                                         d    p                    $             |   �            �   �          �            �
       �    p  T  �      �       4   �����   �       
               �       
                     
                   � ߱            $  q  d  ���                       x    u           @      4   ����@      $  u  L  ���                       h                         � ߱        �    v  �  �      t      4   ����t      $  v  �  ���                       �                         � ߱        4    �    (  �  �      4   �����      o   �  
     \                              �  �  NA  �  �    �       ,     @    T    h    |    �    �  `  �  
`  �  $  �    �           $  �    ���                            
 
                   � ߱        d�    �  P  �      $      4   ����$                �                      ��                  �  �                  ؞p                       �  `  `    �  �        X      4   ����X      $  �  4  ���                       �  @         �              � ߱              �  |  �      �      4   �����      $  �  �  ���                       @  @         ,              � ߱        assignPageProperty                              |  d      ��                  Q  T  �              ܧ�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            changePage                              �  �      ��                  V  W  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �  �      ��                  Y  [  �              L��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            constructObject                             �  �      ��                  ]  b                @��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   \             (               �� 
  �             P  
             ��   �             x               �� 
                 �  
         ��                            ����                            createObjects                               �  �      ��                  d  e  �              ��z                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �  �      ��                  g  i  �              �z                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            destroyObject                               �  �      ��                  k  l  �              ��z                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �  �      ��                  n  p  �              D�z                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  �      ��                  r  s                � {                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                                 �      ��                  u  v                 �{                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                                 �      ��                  x  z                 @{                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8           ��                            ����                            notifyPage                              0        ��                  |  ~  H              �+�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `           ��                            ����                            passThrough                             X  @      ��                  �  �  p              0�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            removePageNTarget                               �  �      ��                  �  �  �               8�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                �  
             ��                              ��                            ����                            selectPage                               !  �       ��                  �  �  !              ,@�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  0!           ��                            ����                            toolbar                             $"  "      ��                  �  �  <"              ,�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  T"           ��                            ����                            viewObject                              L#  4#      ��                  �  �  d#              \�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                L$  4$      ��                  �  �  d$               �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  |$           ��                            ����                            disablePagesInFolder    
      �$      %    i	      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �$      H%      |%    ~	      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  \%      �%      �%    �	      HANDLE, getCallerWindow �%      �%      &    �	      HANDLE, getContainerMode    �%      &      P&    �	      CHARACTER,  getContainerTarget  0&      \&      �&    �	      CHARACTER,  getContainerTargetEvents    p&      �&      �&    �	      CHARACTER,  getCurrentPage  �&      �&      '    �	      INTEGER,    getDisabledAddModeTabs  �&       '      X'     
      CHARACTER,  getDynamicSDOProcedure  8'      d'      �'  !  
      CHARACTER,  getFilterSource |'      �'      �'  "  /
      HANDLE, getMultiInstanceActivated   �'      �'      (  #  ?
      LOGICAL,    getMultiInstanceSupported   �'      ((      d(  $  Y
      LOGICAL,    getNavigationSource D(      p(      �(  %  s
      CHARACTER,  getNavigationSourceEvents   �(      �(      �(  &  �
      CHARACTER,  getNavigationTarget �(      �(      ,)  '  �
      HANDLE, getOutMessageTarget )      4)      h)  (  �
      HANDLE, getPageNTarget  H)      p)      �)  )  �
      CHARACTER,  getPageSource   �)      �)      �)  *  �
      HANDLE, getPrimarySdoTarget �)      �)      *  +  �
      HANDLE, getReEnableDataLinks    �)       *      X*  ,  �
      CHARACTER,  getRunDOOptions 8*      d*      �*  -        CHARACTER,  getRunMultiple  t*      �*      �*  .        LOGICAL,    getSavedContainerMode   �*      �*      +  /  .      CHARACTER,  getSdoForeignFields �*       +      T+  0  D      CHARACTER,  getTopOnly  4+      `+      �+  1 
 X      LOGICAL,    getUpdateSource l+      �+      �+  2  c      CHARACTER,  getUpdateTarget �+      �+      ,  3  s      CHARACTER,  getWaitForObject    �+      ,      D,  4  �      HANDLE, getWindowTitleViewer    $,      L,      �,  5  �      HANDLE, getStatusArea   d,      �,      �,  6  �      LOGICAL,    pageNTargets    �,      �,      �,  7  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �,      0-      `-  8  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  @-      x-      �-  9  �      LOGICAL,INPUT h HANDLE  setCallerWindow �-      �-      �-  :  �      LOGICAL,INPUT h HANDLE  setContainerMode    �-      .      @.  ;  �      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget   .      h.      �.  <        LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  |.      �.      �.  =        LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �.      /      D/  >  *      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  $/      t/      �/  ?  A      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �/      �/      �/  @  X      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �/      0      P0  A  h      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   00      p0      �0  B  {      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �0      �0      1  C  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �0      H1      |1  D  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   \1      �1      �1  E  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �1       2      42  F  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget 2      T2      �2  G  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  h2      �2      �2  H        LOGICAL,INPUT pcObject CHARACTER    setPageSource   �2      �2      ,3  I        LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget 3      L3      �3  J  "      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    `3      �3      �3  K  6      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �3      4      <4  L  K      LOGICAL,INPUT phObject HANDLE   setRunDOOptions 4      \4      �4  M  [      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  l4      �4      �4  N  k      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �4      5      <5  O  z      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields 5      h5      �5  P  �      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  |5      �5      �5  Q 
 �      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �5      6      D6  R  �      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget $6      h6      �6  S  �      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    x6      �6      �6  T  �      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �6      7      H7  U  �      LOGICAL,INPUT phViewer HANDLE   getObjectType   (7      h7      �7  V  �      CHARACTER,  setStatusArea   x7      �7      �7  W        LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �8  p8      ��                      �8              �'~                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �9  t9      ��                      �9              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �:  x:      ��                      �:              H                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �;  �;      ��                      �;              `                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �<  �<      ��                    !  �<              @                     O   ����    e�          O   ����    R�          O   ����    ��            ��                  �<           ��                            ����                            getAllFieldHandles  �7      4=      h=  X        CHARACTER,  getAllFieldNames    H=      t=      �=  Y  $      CHARACTER,  getCol  �=      �=      �=  Z  5      DECIMAL,    getDefaultLayout    �=      �=      >  [  <      CHARACTER,  getDisableOnInit    �=      (>      \>  \  M      LOGICAL,    getEnabledObjFlds   <>      h>      �>  ]  ^      CHARACTER,  getEnabledObjHdls   |>      �>      �>  ^  p      CHARACTER,  getHeight   �>      �>      ?  _ 	 �      DECIMAL,    getHideOnInit   �>       ?      P?  `  �      LOGICAL,    getLayoutOptions    0?      \?      �?  a  �      CHARACTER,  getLayoutVariable   p?      �?      �?  b  �      CHARACTER,  getObjectEnabled    �?      �?      @  c  �      LOGICAL,    getObjectLayout �?      @      L@  d  �      CHARACTER,  getRow  ,@      X@      �@  e  �      DECIMAL,    getWidth    `@      �@      �@  f  �      DECIMAL,    getResizeHorizontal �@      �@      �@  g  �      LOGICAL,    getResizeVertical   �@      A      8A  h        LOGICAL,    setAllFieldHandles  A      DA      xA  i        LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    XA      �A      �A  j  '      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �A      �A       B  k  8      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit     B      DB      xB  l  I      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   XB      �B      �B  m  Z      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �B      �B      C  n  h      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �B      @C      pC  o  y      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal PC      �C      �C  p  �      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �C      �C      (D  q  �      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated D      PD      �D  r  �      LOGICAL,    getObjectSecured    dD      �D      �D  s  �      LOGICAL,    createUiEvents  �D      �D       E  t  �      LOGICAL,    bindServer                              �E  �E      ��                      �E              DY�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �F  �F      ��                      �F              �Y�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �G  �G      ��                  	  
  �G              8f�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �H  �H      ��                      �H              �f�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �I  �I      ��                      �I              <g�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �J  �J      ��                      �J              8j�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �K  �K      ��                      �K              �j�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �K  
         ��                            ����                            startServerObject                               �L  �L      ��                      M              @o�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �M  �M      ��                      N              8r�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  ,N           ��                            ����                            getAppService   �D      �N      �N  u  �      CHARACTER,  getASBound  �N      �N      �N  v 
 �      LOGICAL,    getAsDivision   �N      O      8O  w  �      CHARACTER,  getASHandle O      DO      pO  x  
      HANDLE, getASHasStarted PO      xO      �O  y        LOGICAL,    getASInfo   �O      �O      �O  z 	 &      CHARACTER,  getASInitializeOnRun    �O      �O      $P  {  0      LOGICAL,    getASUsePrompt  P      0P      `P  |  E      LOGICAL,    getServerFileName   @P      lP      �P  }  T      CHARACTER,  getServerOperatingMode  �P      �P      �P  ~  f      CHARACTER,  runServerProcedure  �P      �P      $Q    }      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   Q      hQ      �Q  �  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   xQ      �Q      �Q  �  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �Q      R      @R  �  �      LOGICAL,INPUT phASHandle HANDLE setASInfo    R      `R      �R  � 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    lR      �R      �R  �  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �R      S      8S  �  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   S      XS      �S  �  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  lS      �S      �S  �  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �T  �T      ��                  �  �  �T              Ns                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  U             �T  
             ��   0U             �T               �� 
                 $U  
         ��                            ����                            addMessage                              V  V      ��                  �  �  4V              $\s                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �V             LV               ��   �V             tV               ��                  �V           ��                            ����                            adjustTabOrder                              �W  �W      ��                  �  �  �W              �cs                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �W             �W  
             �� 
  $X             �W  
             ��                  X           ��                            ����                            applyEntry                              Y  �X      ��                  �  �  (Y              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @Y           ��                            ����                            changeCursor                                <Z  $Z      ��                  �  �  TZ              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  lZ           ��                            ����                            createControls                              h[  P[      ��                  �  �  �[              @�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               l\  T\      ��                  �  �  �\              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                p]  X]      ��                      �]              $�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              |^  d^      ��                      �^              Ԧ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              |_  d_      ��                      �_              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              |`  d`      ��                  
    �`              t�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �a  la      ��                      �a              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �b  tb      ��                      �b              \�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �b             �b  
             ��   c             �b               ��   @c             c               ��                  4c           ��                            ����                            modifyUserLinks                             0d  d      ��                      Hd              `�{                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �d             `d               ��   �d             �d               �� 
                 �d  
         ��                            ����                            removeAllLinks                              �e  �e      ��                      �e              ��{                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �f  �f      ��                     $  �f              ��{                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  g             �f  
             ��   8g             g               �� 
                 ,g  
         ��                            ����                            repositionObject                                ,h  h      ��                  &  )  Dh              �{                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �h             \h               ��                  �h           ��                            ����                            returnFocus                             |i  di      ��                  +  -  �i              d�{                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �i  
         ��                            ����                            showMessageProcedure                                �j  �j      ��                  /  2  �j              d�{                    O   ����    e�          O   ����    R�          O   ����    ��            ��   k             �j               ��                  k           ��                            ����                            toggleData                               l  �k      ��                  4  6  l              4�{                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  0l           ��                            ����                            viewObject                              (m  m      ��                  8  9  @m              ��{                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �S      �m      �m  � 
 ]      LOGICAL,    assignLinkProperty  �m      �m      n  �  h      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �m      \n      �n  �  {      CHARACTER,  getChildDataKey ln      �n      �n  �  �      CHARACTER,  getContainerHandle  �n      �n      o  �  �      HANDLE, getContainerHidden  �n      o      Do  �  �      LOGICAL,    getContainerSource  $o      Po      �o  �  �      HANDLE, getContainerSourceEvents    do      �o      �o  �  �      CHARACTER,  getContainerType    �o      �o      p  �  �      CHARACTER,  getDataLinksEnabled �o      p      Hp  �  �      LOGICAL,    getDataSource   (p      Tp      �p  �        HANDLE, getDataSourceEvents dp      �p      �p  �        CHARACTER,  getDataSourceNames  �p      �p       q  �  2      CHARACTER,  getDataTarget   �p      q      <q  �  E      CHARACTER,  getDataTargetEvents q      Hq      |q  �  S      CHARACTER,  getDBAware  \q      �q      �q  � 
 g      LOGICAL,    getDesignDataObject �q      �q      �q  �  r      CHARACTER,  getDynamicObject    �q       r      4r  �  �      LOGICAL,    getInstanceProperties   r      @r      xr  �  �      CHARACTER,  getLogicalObjectName    Xr      �r      �r  �  �      CHARACTER,  getLogicalVersion   �r      �r      �r  �  �      CHARACTER,  getObjectHidden �r      s      8s  �  �      LOGICAL,    getObjectInitialized    s      Ds      |s  �  �      LOGICAL,    getObjectName   \s      �s      �s  �  �      CHARACTER,  getObjectPage   �s      �s      �s  �        INTEGER,    getObjectParent �s       t      0t  �        HANDLE, getObjectVersion    t      8t      lt  �  %      CHARACTER,  getObjectVersionNumber  Lt      xt      �t  �  6      CHARACTER,  getParentDataKey    �t      �t      �t  �  M      CHARACTER,  getPassThroughLinks �t      �t      0u  �  ^      CHARACTER,  getPhysicalObjectName   u      <u      tu  �  r      CHARACTER,  getPhysicalVersion  Tu      �u      �u  �  �      CHARACTER,  getPropertyDialog   �u      �u      �u  �  �      CHARACTER,  getQueryObject  �u       v      0v  �  �      LOGICAL,    getRunAttribute v      <v      lv  �  �      CHARACTER,  getSupportedLinks   Lv      xv      �v  �  �      CHARACTER,  getTranslatableProperties   �v      �v      �v  �  �      CHARACTER,  getUIBMode  �v       w      ,w  � 
 �      CHARACTER,  getUserProperty w      8w      hw  �        CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    Hw      �w      �w  �        CHARACTER,INPUT pcPropList CHARACTER    linkHandles �w      �w      x  �  (      CHARACTER,INPUT pcLink CHARACTER    linkProperty    �w      @x      px  �  4      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry Px      �x      �x  �  A      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �x      Dy      ty  �  M      CHARACTER,INPUT piMessage INTEGER   propertyType    Ty      �y      �y  �  [      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  �y      �y       z  �  h      CHARACTER,  setChildDataKey  z      ,z      \z  �  w      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  <z      �z      �z  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  �z      �z      {  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �z      ,{      h{  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled H{      �{      �{  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   �{      �{      |  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �{      8|      l|  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  L|      �|      �|  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   �|      �|       }  �        LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents  }      D}      x}  �        LOGICAL,INPUT pcEvents CHARACTER    setDBAware  X}      �}      �}  � 
 1      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject �}      �}      ~  �  <      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    �}      D~      x~  �  P      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   X~      �~      �~  �  a      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    �~      �~      (  �  w      LOGICAL,INPUT c CHARACTER   setLogicalVersion         D      x  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   X      �      �  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent �      �      �  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    �      <�      p�  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    P�      ��      ̀  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      �      (�  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �      H�      ��  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  `�      ��      ԁ  �        LOGICAL,INPUT cVersion CHARACTER    setRunAttribute ��      ��      (�  �        LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �      P�      ��  �  +      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   d�      ��      �  �  =      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  Ă      �      4�  � 
 W      LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      T�      ��  �  b      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage d�      ă      ��  �  r      LOGICAL,INPUT pcMessage CHARACTER   Signature   Ѓ      �      @�  � 	 ~      CHARACTER,INPUT pcName CHARACTER    8�    O	  ��  ��      p      4   ����p                �                      ��                  P	  }	                  �D�                       P	  ��        Q	  (�  ��      �      4   �����                ��                      ��                  R	  |	                   E�                       R	  8�  ��    i	  Ѕ  L�      �      4   �����                \�                      ��                  u	  w	                  �E�                       u	  ��         v	                                  0     
 
 4       4           � ߱        ��  $  y	  ��  ���                           $  {	  �  ���                       |      
 5       5           � ߱        D�    �	  T�  Ї      �      4   �����                ��                      ��                  �	  F
                  XF�                       �	  d�  �  o   �	  
 3   ,                                 l�  $   �	  @�  ���                          @         �              � ߱        ��  �   �	         ��  �   �	  �      ��  �   �	        ��  �   �	  |      Ј  �   �	  �      �  �   �	  d      ��  �   �	  �      �  �   �	  	       �  �   �	  �	      4�  �   �	  
      H�  �   �	  �
      \�  �   �	  �
      p�  �   �	  x      ��  �   �	  �      ��  �   �	  0      ��  �   �	  �      ��  �   �	  �      ԉ  �   �	  T      �  �   �	  �      ��  �   �	        �  �   �	  x      $�  �   �	  �      8�  �   �	  p      L�  �   �	  �      `�  �   �	  `      t�  �   �	  �      ��  �   �	  H      ��  �   �	  �      ��  �   �	  �      Ċ  �   �	  4      ؊  �   �	  �      �  �   �	  �       �  �   �	         �  �   �	  \      (�  �   �	  �      <�  �   �	        P�  �   �	  P      d�  �   �	  �      x�  �   �	  �      ��  �   �	        ��  �   �	  @      ��  �   �	  |      ȋ  �   �	  �      ܋  �   �	  �          �   �	  0                      �          t�  \�      ��                  m
  �
  ��              \J�                    O   ����    e�          O   ����    R�          O   ����    ��      �     
 
 2       2             
 6       6       ,                          � ߱        4�  $ �
  ��  ���                           O   �
  ��  ��  l                ��          ��  ��    ��                                              ��                            ����                                h7      ��      L�      6     ��                      V ��  �                     �    �
  `�  ܎      x      4   ����x                �                      ��                  �
  B                  �_�                       �
  p�   �  �   �
  �      �  �   �
  L      (�  �   �
  �      <�  �   �
  D      P�  �   �
  �      d�  �   �
  <      x�  �   �
  �      ��  �   �
  ,      ��  �   �
  �      ��  �   �
  $      ȏ  �   �
  �      ܏  �   �
        ��  �   �
  �          �   �
        ܒ    M   �  ��      |      4   ����|                ��                      ��                  N  �                  �a�                       N  0�  ��  �   P  �      Ԑ  �   Q  P       �  �   R  �       ��  �   S  @!      �  �   T  �!      $�  �   U  ("      8�  �   V  �"      L�  �   W  #      `�  �   X  �#      t�  �   Y   $      ��  �   Z  |$      ��  �   [  �$      ��  �   \  d%      đ  �   ]  �%      ؑ  �   ^  \&      �  �   _  �&       �  �   `  T'      �  �   a  �'      (�  �   b  L(      <�  �   c  �(      P�  �   d  D)      d�  �   e  �)      x�  �   f  <*      ��  �   g  �*      ��  �   h  4+      ��  �   i  �+      Ȓ  �   j  ,,          �   k  �,      ��    �  ��  t�      -      4   ����-                ��                      ��                  �  �                  T                       �  �  ��  �   �  p-      ��  �   �  �-      ��  �   �  h.      ԓ  �   �  �.      �  �   �  P/      ��  �   �  �/      �  �   �  80      $�  �   �  t0      8�  �   �  �0      L�  �   �  $1      `�  �   �  `1      t�  �   �  �1      ��  �   �  H2      ��  �   �  �2      ��  �   �  83      Ĕ  �   �  �3      ؔ  �   �   4      �  �      �4       �  �     5      �  �     T5      (�  �     �5      <�  �     <6      P�  �     �6      d�  �     �6      x�  �     (7      ��  �   	  �7      ��  �   
  �7      ��  �     8      ȕ  �     X8      ܕ  �     �8      �  �     �8      �  �     9      �  �     H9      ,�  �     �9      @�  �     �9      T�  �     4:      h�  �     p:      |�  �     �:      ��  �     �:      ��  �     $;      ��  �     `;      ̖  �     �;      ��  �     H<      ��  �     �<      �  �     0=      �  �     �=      0�  �     (>      D�  �      �>      X�  �   !   ?      l�  �   "  �?      ��  �   #  @      ��  �   $  T@      ��  �   %  �@      ��  �   &  A      З  �   '  HA      �  �   (  �A          �   )  �A      P�  $  �  $�  ���                       `B     
 
 7       7           � ߱        �    �  l�  |�      lB      4   ����lB      /   �  ��     ��                          3   ����|B            ؘ                      3   �����B  <�    �  �  ��  l�  �B      4   �����B  	              ��                      ��             	     �  n                  ���                       �  �  ��  �   �  C      ��  $  �  Й  ���                       DC     
 
 4       4           � ߱        �  �   �  dC      h�  $   �  <�  ���                       �C  @         xC              � ߱        $�  $  �  ��  ���                       �C      
 8       8           � ߱        TD     
 
 2       2       �D      
 6       6        F  @        
 �E              � ߱        ��  V   �  ��  ���                        ,F      
 8       8       `F      
 9       9       �F      
 8       8           � ߱        D�  $    P�  ���                       \G     
 
 2       2       �G      
 6       6       (I  @        
 �H              � ߱        Ԝ  V   -  ��  ���                        4I     
 
 2       2       �I      
 6       6        K  @        
 �J              � ߱            V   R  p�  ���                        
              4�                      ��             
     p                    0��                       p   �  K     
 
 2       2       �K      
 6       6       �L  @        
 �L          <M  @        
 �L          �M  @        
 \M          �M  @        
 �M              � ߱            V   �  |�  ���                        adm-clone-props �  `�              �    ! 7     `                          \  �#                     start-super-proc    p�  ̞  �           �    " 8                                  �#                     ԟ    %  X�  h�      �Q      4   �����Q      /   &  ��     ��                          3   �����Q            ğ                      3   �����Q  ,�  $  @   �  ���                       �Q      
 :       :           � ߱        �    P  H�  Ġ  d�  �Q      4   �����Q                8�                      ��                  Q  U                  4�w                       Q  X�  R      
 :       :       R      
 ;       ;       0R      
 <       <           � ߱            $  R  Ԡ  ���                             V  ��  ��      HR      4   ����HR  hR      
 :       :           � ߱            $  W  ��  ���                       �    ^  �  �  l�  |R      4   ����|R      $  _  @�  ���                       �R      
 <       <           � ߱            �   |  �R      �R     
 
 2       2       lS      
 6       6       �T  @        
 |T              � ߱        �  V   �  ��  ���                        $�  �   �  �T      ��    E  @�  P�      U      4   ����U      /   F  |�     ��                          3   ����U            ��                      3   ����8U  x�  $  J  �  ���                       TU      
 =       =           � ߱        �U     
 
 2       2       �U      
 6       6       LW  @        
 W              � ߱        ��  V   T  �  ���                        ��    �  ��  <�      XW      4   ����XW                L�                      ��                  �  �                  �]�                       �  Ф      g   �  d�         ��(�                           ,�          ��  �      ��                  �      �              ,^�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  X�     h�  �W                      3   ����hW  ��     
   ��                      3   �����W         
   ��                      3   �����W    ��                              ��        	                  ����                                        x�              9      Ȧ                      g                               ��  g   �  ��          ��	0�                           d�          4�  �      ��                  �  �  L�              �^�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  �W                      3   �����W            ��                      3   �����W    ��                              ��        	                  ����                                        ��              :      Ш                      g                               ��  g   �  ��          ��	8�                           l�          <�  $�      ��                  �  �  T�              d_�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  �W                      3   �����W            Ȫ                      3   ���� X    ��                              ��        	                  ����                                        ��              ;      ت                      g                               ��    �  ��  ,�      X      4   ����X                <�                      ��                  �                    訊                       �  ��  ��  /   �  h�     x�                          3   ����,X            ��                      3   ����LX  ��  /  �  Ԭ     �  �X                      3   ����hX  �     
   �                      3   �����X  D�        4�                      3   �����X  t�        d�                      3   �����X            ��                      3   �����X  ̮    �  ��  Э      �X      4   �����X      /    ��     �  |Y                      3   ����\Y  <�     
   ,�                      3   �����Y  l�        \�                      3   �����Y  ��        ��                      3   �����Y            ��                      3   �����Y        
  �  ��      �Y      4   �����Y      /    $�     4�  8Z                      3   ����Z  d�     
   T�                      3   ����@Z  ��        ��                      3   ����HZ  į        ��                      3   ����\Z            �                      3   ����xZ  ��      �  ��      �Z      4   �����Z                ��                      ��                                      ج�                          �      g     ��         ��X�        �Z                  |�          L�  4�      ��                        d�              D��                    O   ����    e�          O   ����    R�          O   ����    ��          /    ��     ��  �Z                      3   �����Z  �     
   ر                      3   �����Z         
   �                      3   �����Z    ��                            ����                                        Ȱ              <      �                      g                               L�       �Z                                      [     
 
 2       2       |[      
 6       6       �\  @        
 �\              � ߱        ܳ  V   �  �  ���                        �\     
 
 2       2       \]      
 6       6       �^  @        
 l^              � ߱        �  V   �  x�  ���                        ��      $�  4�      �^      4   �����^      $   	  `�  ���                        _  @         _              � ߱        `�  g     ��         ���        4_  ���        @_                  ��          P�  8�      ��                      h�              `c�                    O   ����    e�          O   ����    R�          O   ����    ��              ��  ��      L_      4   ����L_      O    ������  `_    ��                            ����                                        ̴              =      ĵ                      g                               �  g   #  x�         �6��         t_                  @�          �  ��      ��                  $  )  (�              �c�                    O   ����    e�          O   ����    R�          O   ����    ��      X�    '  �_  }          O  (  ������  �_    ��                            ����                                        ��              >      p�                      g                               ��  g   1  $�         �"d�                           �          ��  ��      ��                  2  <  Ը              0��                    O   ����    e�          O   ����    R�          O   ����    ��            :  �_  }        ��                              ��        	                  ����                                        8�              ?      �                      g                               ��  g   D  ع         �"��                            t�          p�  X�      ����               E  �  ��              į�                    O   ����    e�          O   ����    R�          O   ����    ��             
 +       +              
 ,       ,              
 -       -              
 .       .              
 0       0              
 1       1              
 /       /           � ߱        ��  $   F  ��   �                              
                      
                      
                      
                      
 !       !              
 "       "              
 $       $              
 %       %           � ߱        �  $   N  ��   �                              
                      
 *       *           � ߱        0�  $   X  ��   �                       4�    \  L�  Ƚ      �_      4   �����_                ؽ                      ��                  a  d                  ���                       a  \�  �  	  b  �                                        3   ���� `      O  c  ������  ,`  �  /   s  `�     p�                          3   ����@`  ��        ��  ��                  3   ����h`      $   s  ̾  ���                                                   � ߱        ��        �  (�                  3   ����t`      $   s  T�  ���                                                   � ߱                  ��  ��                  3   �����`      $   s  ܿ  ���                                                   � ߱        L�    t  $�  4�      �`      4   �����`      O  t  ������  �`  `�  �   v  �`      ��  $   w  ��  ���                       �`  @         �`              � ߱        ��  /   x  ��                                 3   ���� a  0�  /   y   �                                 3   ����a  ��  $   z  \�  ���                       Da  @         0a              � ߱        ��  �   {  Pa      H�  A  }        ��   ��                                                                 4�  (�                                   @            �   �    L�    ~  d�  ��      pa      4   ����pa                ��                      ��                  ~  �                  (2�                       ~  t�  4�  	    $�                                        3   ����|a      O  �  ������  �a  ��  $  �  x�  ���                       �a                         � ߱        ��  $  �  ��  ���                       b                         � ߱        �  �   �  @b      h�  $   �  <�  ���                       tb  @         `b              � ߱        4�  /   �  ��     ��                          3   �����b  ��     
   ��                      3   �����b  �        ��                      3   �����b            $�                      3   �����b  ��  $   �  `�  ���                       �b  @         �b              � ߱        ��  �   �  �b      $�    �  ��  ��      c      4   ����c      $  �  ��  ���                       @c                         � ߱        ��    �  @�  P�      lc      4   ����lc      $  �  |�  ���                       �c                         � ߱         �  $  �  ��  ���                       �c                         � ߱        (�  w  �     �          3   ����d  L�    �  D�  ��      (d      4   ����(d                ��                      ��                  �  �                  $3�                       �  T�  4�  	  �  �                                    �  3   ����Td  $�  3   ����`d      3   ����td      O   �  ��  ��  �d  t�  w   �     d�          3   �����d  ��    �  ��  ��      �d      4   �����d      $  �  ��  ���                       �d                         � ߱        |�    �  �  $�      e      4   ����e      $  �  P�  ���                       De                         � ߱        ��  $  �  ��  ���                       pe                         � ߱        ��  w  �     ��          3   �����e  l�  w   �     �          3   �����e  �e      
 $       $        f      
 %       %           � ߱        ��  $  �  $�  ���                       f  �          f  �              � ߱        ��  Z   �  ��   �                            	  �  (�                                        3   ����$f    ��                              ��        	                  ����                                              �              @      8�                      g                               ��  g   �  �         �"\�        
                   ��          ��  ��      ��                  �  �  ��              �f�                    O   ����    e�          O   ����    R�          O   ����    ��      �  r   �          # $ 
         Tf      �  0f    <f  Hf        �  8�  H�      `f      4   ����`f      $   �  t�  ���                       �f  @         �f              � ߱                   #  ��          ��  ��    ��                                        #     ��                              ��        	                  ����                            ��          (�      ��    # A     ��                      g   ��                          t�  g   �  ��         �"�                           ��          h�  P�      ��                  �  �  ��              tk�                    O   ����    e�          O   ����    R�          O   ����    ��      ��  r   �          $ % 
         �f      ��  �f    �f  �f        �  ��  �      �f      4   �����f      $   �  0�  ���                       g  @          g              � ߱                   $  ��          ��  ��    ��                                        $     ��                              ��        	                  ����                            \�          ��      \�    $ B     ��                      g   ��                          ��  g   �  ��         �48�                           T�          $�  �      ��                 �  �  <�              X#]                    O   ����    e�          O   ����    R�          O   ����    ��      h�  �   �   g      |�  �   �  Hg      ��  $   �  ��  ���                       |g  @         hg              � ߱              �  ��  l�      �g      4   �����g                ��                      ��                  �  �                  8&]                       �   �        ��      ��          t�  \�      ��                  �  �  ��              �&]                       �  |�      $�  t�       ��                            7   ����    %      ��                     �            ��                  6   �       %  �   ��         ��        �            ��                                                        �g   �g   �g                 H�  <�           �g           h                      �   ,�        O   ����  e�          O   ����  R�          O   ����  ��          �   �  h        ��                              ��        	                   ��                            ����                                        ��              C      ��                      g                               ��  g   �  ��         � ��                           t�          D�  ,�      ����               �    \�              �*]                    O   ����    e�          O   ����    R�          O   ����    ��      ��    �  ��  ��      Th      4   ����Th      O   �  ��  ��  |h  p�  A  �       & �   ��         �                                            �h                 \�  P�           �h           �h         �            0�   @�    ��    �  ��  �      �h      4   �����h                �                      ��                  �                    �ґ                       �  ��  \�  	     L�                                        3   �����h  ��  $     ��  ���                       �h  @         �h              � ߱            O    ������  �h      $     ��  ���                       i  @          i              � ߱          ��                              ��        	                  ����                                &              ��              D      $�                      g                               ��  g      �         ��,�            �4,�                           ��          ��  ��      ��                      ��              tӑ                    O   ����    e�          O   ����    R�          O   ����    ��      H�  /     �     �                          3   ���� i            8�                      3   ����<i          d�  t�      Hi      4   ����Hi      $     ��  ���                       �i  @         pi              � ߱          ��                              ��        	                  ����                                        (�              E      ��                      g                               ��  g     ��         � ��                           h�          8�   �      ��x�                 $  P�              X֑                    O   ����    e�          O   ����    R�          O   ����    ��      ��      ��  ��      �i      4   �����i      O     ��  ��  �i  d�  A         ' �   ��         ��                                            �i                 P�  D�           �i           �i         �            $�   4�    ��      ��  ��      �i      4   �����i                �                      ��                    "                  �ڑ                         ��  P�  	    @�                                        3   �����i  ��  $      |�  ���                       j  @         j              � ߱            O  !  ������  (j      $   #  ��  ���                       Pj  @         <j              � ߱          ��                              ��        	                  ����                                '              ��              F      �                      g                               0�  g   ,  ��         � ��                           ��          ��  t�      ����               -  6  ��              (ۑ                    O   ����    e�          O   ����    R�          O   ����    ��       �    .  ��  ��      \j      4   ����\j      O   .  ��  ��  �j  ��  A  /       ( \�   ��         P�                                            �j                 ��  ��           �j           �j         �            x�   ��    �    0  ��  P�      �j      4   �����j                `�                      ��                  0  4                  �#v                       0  ��  ��  	  1  ��                                        3   �����j  ��  $   2  ��  ���                       �j  @         �j              � ߱            O  3  ������  �j      $   5  @�  ���                       k  @         k              � ߱          ��                              ��        	                  ����                                (              �              G      l�                      g                               ��  g   =  H�         ��`�                           �          ��  ��      ��                  >  B  ��              �&v                    O   ����    e�          O   ����    R�          O   ����    ��      |�  /   ?  <�     L�                          3   ����(k            l�                      3   ����Dk        @  ��  ��      Pk      4   ����Pk      $   @  ��  ���                       �k  @         xk              � ߱          ��                              ��        	                  ����                                        \�              H       �                      g                               �  g   J  ��         � ��                           ��          l�  T�      ����               K  T  ��              |'v                    O   ����    e�          O   ����    R�          O   ����    ��      ��    L  ��  ��      �k      4   �����k      O   L  ��  ��  �k  ��  A  M       ) <�   ��         0�                                            �k                 ��  x�           �k           �k         �            X�   h�    ��    N  ��  0�      �k      4   �����k                @�                      ��                  N  R                  �+v                       N  ��  ��  	  O  t�                                        3   ����l  ��  $   P  ��  ���                       $l  @         l              � ߱            O  Q  ������  0l      $   S   �  ���                       Xl  @         Dl              � ߱          ��                              ��        	                  ����                                )              ��              I      L�                      g                               l�  g   \  (�         �4�                           ��          ��  ��      ��                  ]  g  ��              (R                    O   ����    e�          O   ����    R�          O   ����    ��      H�  $   ^  �  ���                       xl  @         dl              � ߱        ��  $   _  t�  ���                       �l  @         �l              � ߱        ��    `  ��  ��  l�  �l      4   �����l   m  @         �l              � ߱            $   a  ��  ���                       (m  @         m          Pm  @         <m              � ߱            $   c  $�  ���                             f  \m         ��                              ��        	                  ����                                        <�              J      ��                      g                               `�  g   o  ��         �4�                           L�          �  �      ��                  p  r  4�              �R                    O   ����    e�          O   ����    R�          O   ����    ��          $   q  x�  ���                       |m  @         hm              � ߱          ��                              ��        	                  ����                                        ��              K      ��                      g                               $�  g   z  x�         �4��                           @�          �  ��      ��                  {  �  (�              HS                    O   ����    e�          O   ����    R�          O   ����    ��      ��  $   |  l�  ���                       �m  @         �m              � ߱        ��  $   }  ��  ���                       �m  @         �m              � ߱        H�  $   ~  �  ���                        n  @         �m              � ߱        ��  $     t�  ���                       ,n  @         n              � ߱        ��  $   �  ��  ���                       Xn  @         Dn              � ߱        P�  $   �  $�  ���                       �n  @         pn              � ߱              �  �n         ��                              ��        	                  ����                                        ��              L      h�                      g                               �  g   �  <�         �4��                           �          ��  ��      ��                  �  �  ��              @V                    O   ����    e�          O   ����    R�          O   ����    ��          $   �  0�  ���                       �n  @         �n              � ߱          ��                              ��        	                  ����                                        P�              M      \�                      g                               ��  g   �  0�         �4H�                           x�          ��  ��      ��                  �  �  ��              �V                    O   ����    e�          O   ����    R�          O   ����    ��             
 +       +              
 -       -              
 0       0              
 1       1           � ߱        ��  $   �  ��   �                       ��    �  ��  �  p�  �n      4   �����n  �o  @         �o          �o  @         �o              � ߱            $   �  ��  ���                       �o  @         �o              � ߱            $   �  D�  ���                             �  ��  H�  ��  �o      4   �����o  hp  @         Tp          �p  @         tp          �p  @         �p          �p  @         �p              � ߱            $   �  ��  ���                       �p  @         �p           q  @         q              � ߱            $   �  t�  ���                         ��                              ��        	                  ����                                        D�              N      ��                      g                               X�  g   �  ��         �4��                           ��          T�  <�      ��                  �  �  l�              (Z                    O   ����    e�          O   ����    R�          O   ����    ��            �  4q         ��                              ��        	                  ����                                        ��              O      ��                      g                                     �  t�  ��      @q      4   ����@q                d�                      ��                  �  �                  �\                       �  ��  Pq  @                     |q  @         hq          �q  @         �q              � ߱        ��  $   �   �  ���                       � g   �  ��         �n0     }                      p          @  (      ��                  �  �  X              x�\                    O   ����    e�          O   ����    R�          O   ����    ��      �  /  �  �                                 3   �����q        �  �  �      �q      4   �����q      O  �  ������   r    ��                            ����                                        ��              P      �                      g                               ` g   �  �        �!        r                  �         < $     ��                  �  �  T             $�\                    O   ����    e�          O   ����    R�          O   ����    ��       r  @                         � ߱            $  �  l ���                         ��                            ����                                        �             Q      �                     g                               � /   �  �                                3   ����(r        �  � 4     Dr      4   ����Dr                �                     ��                  �  �                  ��\                       �  �               �         � �     ��                 �  �                  p�\                       �  D     O   �    ��          O   �    ��      , /   �                                  3   ����\r        �  H X     |r      4   ����|r      k   �  t             }       n        �   adm-create-objects  �  �                     R      �                               (                     Carga-Lista-Articulos   � �                     S                                    (                     Carga-Lista-Clientes     p         8      *   T     t                          p  5(                     Carga-Temporal  � �         t6  �6  , + U      7                          7  �)                     disable_UI  � P                     V      <                              �)  
                   enable_UI   \ �                     W      �                              �)  	                   Excel-2 �               �(    . X     ()                          $)  !*                     exitObject  ( �                     Y      �                               )*  
                   initializeObject    � �         �      3   Z     �                          �  �*                     Resumen-General  	 \	                     [      �i                              g+                     Resumen-por-cliente l	 �	                     \      �G                              �+                     Resumen-por-climat  �	 8
                     ]      �Z                              �+                     Resumen-por-division    L
 �
                     ^      �                              5,                     Resumen-por-familia �
              l    > _     �                          �  n,                     Resumen-por-producto    0 �                     `      �$                              �,                     Resumen-por-resmat  �                       a      T"                              �,                     Resumen-por-vendcli  p                     b      �L                              O-                     Resumen-por-vendedor    � �                     c      t                              z-                     �  @ V �   �  �     ��     ��     ��     ��     ��     ��     ��  +COSTO       ���������   �   ) 9                    ��   � TodosTodos�           �      ���  �         , 8   ����A   < 8   ����A   L 8   ����@   \ 8   ����@   l 8   ����?   | 8   ����?   � 8   ����	   � 8   ����	   � 	  � 8   ����=   � 8   ����=   � 8   ����<   � 8   ����<   \ <  � 8   ����;    8   ����;    8   ����:   , 8   ����:   < 8   ����9   L 8   ����9   d 8   ����8   t 8   ����8   � 8  � 8   ����7   � 8   ����7   � 7  � 8   ����6   � 8   ����6   T 6  � 8   ����5   � 8   ����5   � 8   ����4    8   ����4    8   ����1   $ 8   ����1   4 8   ����0   D 8   ����0   \ 8   ����/   l 8   ����/   | /  � 8   ����-   � 8   ����-   � -  � 8   ����   � 8   ����   �   � 8   ����   � 8   ����   �  
 � 8   ����    8   ����     	 $ 8   ����   4 8   ����   D   L 8   ����)   \ 8   ����)   l )  t 8   ����(   � 8   ����(   � (  � 8   ����'   � 8   ����'   � '  � 8   ����&   � 8   ����&    &  � 8   ����%   � 8   ����%    8   ����    8   ����             8   ����       8   ����       < H     toggleData  ,INPUT plEnabled LOGICAL    , t �     showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  d � �     returnFocus ,INPUT hTarget HANDLE   �       repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    � T `     removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE D � �     removeAllLinks  ,   � � �     modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE � @ T     modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    0 � �     hideObject  ,   � �      editInstanceProperties  ,   �  (     displayLinks    ,    < L     createControls  ,   , ` p     changeCursor    ,INPUT pcCursor CHARACTER   P � �     applyEntry  ,INPUT pcField CHARACTER    � � �     adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER � < H     addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER , � �     addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE � �      unbindServer    ,INPUT pcMode CHARACTER � 4 H     startServerObject   ,   $ \ l     runServerObject ,INPUT phAppService HANDLE  L � �     restartServerObject ,   � � �     initializeServerObject  ,   � �       disconnectObject    ,   �  (     destroyServerObject ,    < H     bindServer  ,   , \ l     processAction   ,INPUT pcAction CHARACTER   L � �     enableObject    ,   � � �     disableObject   ,   � � �     applyLayout ,   �        viewPage    ,INPUT piPageNum INTEGER    � 8 D     viewObject  ,   ( X `     toolbar ,INPUT pcValue CHARACTER    H � �     selectPage  ,INPUT piPageNum INTEGER    | � �     removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER �        passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER   h t     notifyPage  ,INPUT pcProc CHARACTER X � �     initPages   ,INPUT pcPageList CHARACTER � � �     initializeVisualContainer   ,   �       hidePage    ,INPUT piPageNum INTEGER    � < L     destroyObject   ,   , ` l     deletePage  ,INPUT piPageNum INTEGER    P � �     createObjects   ,   � � �     constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE � @ L     confirmExit ,INPUT-OUTPUT plCancel LOGICAL  0 | �     changePage  ,   l � �     assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 %     adecomm/as-utils.w  
"   
   �    }        �
"     
   � �   � "     � &    &    &    &        %              %              *    4         %              4         %              4         %                  " 
   �%              � M         " 
   �%               � �     %                  �     }        �G� A	   �G%              � E	  #   %         %       %        %       %        %       %               %               %               %              %              %              %               %              
�        
" 
  
 �
�    
" 
  
 �
" 
  
 �    �        @     �        L    
" 
  
   �        �         �     }        �%              
" 
  
 �
" 
  
 �    �        �     �        �    
" 
  
   �                  �     }        �%              � 
"    
 � %              � �  �         X      $              
�    � �   �      
" 
  
 �                      
�            � �   �
"    
 �
�H T   %              �     }        �GG %              � 
" 
 4 
   P �L 
�H T   %              �     }        �GG %              
" 
 3 
   �        �    7%               
" 
 3 
 �               1� �  
 � �   � %               o%   o           � �    �
" 
 3 
 �           �    1� �   � �   � %               o%   o           � �   
" 
 3 
 ��           �    1� �  
 �� �   � %               o%   o           � �   
" 
 3 
 ��           p    1� �   �� �   � %               o%   o           � �   �
" 
 3 
 ��           �    1� �   �� �   � %               o%   o           � �   �
" 
 3 
 ��           X    1�    ��    � %               o%   o           %               
" 
 3 
 � �          �    1� &   � � 6     
" 
 3 
 ��           	    1� =   �� �   � %               o%   o           � P  e �
" 
 3 
 ��           �	    1� �   �� �   � %               o%   o           � �  [ �
" 
 3 
 ��           �	    1� !   ��    � %               o%   o           %               
" 
 3 
 ��           t
    1� 1   ��    � %               o%   o           %               
" 
 3 
 ��           �
    1� C   ��    � %               o%   o           %              
" 
 3 
 � �          l    1� P   � �      
" 
 3 
 ��           �    1� _  
 ��    � %               o%   o           %               
" 
 3 
 ��           $    1� j   �� �   � %               o%   o           � �    �
" 
 3 
 � �          �    1� r   � � 6     
" 
 3 
 {�           �    1� �   {� �   � %               o%   o           � �  t �
" 
 3 
 � �          H    1�   
 � � 6     
" 
 3 
 ��           �    1�    �� �   � %               o%   o           � )  � �
" 
 3 
 ��           �    1� �   �� �   � %               o%   o           � �    �
" 
 3 
 ��           l    1� �  
 �� �   � %               o%   o           %               
" 
 3 
 ��           �    1� �   ��    � %               o%   o           %               
" 
 3 
 ��           d    1� �   �� �   � %               o%   o           � �    �
" 
 3 
 ��           �    1� �   �� �   � %               o%   o           o%   o           
" 
 3 
 ��           T    1�   
 �� �   � %               o%   o           � �    �
" 
 3 
 ��           �    1�    �� !  	 � %               o%   o           � +  / �
" 
 3 
 � �          <    1� [   � � !  	   
" 
 3 
 ��           x    1� m   �� !  	 � o%   o           o%   o           � �    �
" 
 3 
 � �          �    1� �   � � !  	   
" 
 3 
 ��           (    1� �   �� !  	 � o%   o           o%   o           � �    �
" 
 3 
 � �          �    1� �   � �      
" 
 3 
 � �          �    1� �   � � !  	   
" 
 3 
 � �              1� �   � � !  	   
" 
 3 
 � �          P    1� �   � � !  	   
" 
 3 
 ��           �    1� �   ��    � o%   o           o%   o           %              
" 
 3 
 � �              1� �   � � !  	   
" 
 3 
 � �          D    1� �  
 � � �     
" 
 3 
 � �          �    1�    � � !  	   
" 
 3 
 � �          �    1�    � � !  	   
" 
 3 
 � �          �    1� )   � � !  	   
" 
 3 
 � �          4    1� >   � � !  	   
" 
 3 
 � �          p    1� M  	 � � !  	   
" 
 3 
 � �          �    1� W   � � !  	   
" 
 3 
 � �          �    1� j   � � !  	   
" 
 3 
 ��           $    1� �   �� �   � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
" 
 2 
   
" 
 2 
 �
" 
 2 
   
" 
 2 
 �(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�           �� �     p�               �L
�    %              � 8          � $         � �          
�    � �     
" 
 2 
 �� @  , 
�            �� �  
 �p�               �L"       P �L 
�H T   %              �     }        �GG %              
" 
 3 
 ��           �    1� �  
 �� �   � %               o%   o           � �    �
" 
 3 
 ��           @    1� �  
 �� �   � %               o%   o           o%   o           
" 
 3 
 ��           �    1� �   �� 6   � %               o%   o           o%   o           
" 
 3 
 ��           8    1� �   ��    � %               o%   o           %               
" 
 3 
 ��           �    1� �   ��    � %               o%   o           %               
" 
 3 
 ��           0    1� �   �� �   � %               o%   o           � �    �
" 
 3 
 ��           �    1� �   ��    � %               o%   o           %              
" 
 3 
 ��                1�    ��    � %               o%   o           o%   o           
" 
 3 
 ��           �    1�    �� �   � %               o%   o           o%   o           
" 
 3 
 ��               1� (  	 �� �   � %               o%   o           � �    �
" 
 3 
 ��           �    1� 2   �� �   � %               o%   o           o%   o           
" 
 3 
 ��               1� F   �� �   � %               o%   o           o%   o           
" 
 3 
 �           �    1� U   �    � %               o%   o           %               
" 
 3 
 �                1� e   �    � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
" 
 3 
 ��           �    1� q   �� !  	 � %               o%   o           � �    �
" 
 3 
 ��           D     1� ~   �� !  	 � %               o%   o           � �    �
" 
 3 
 ��           �     1� �   ��    � %               o%   o           %               
" 
 3 
 ��           4!    1� �   �� !  	 � %               o%   o           � �    �
" 
 3 
 ��           �!    1� �   �� !  	 � %               o%   o           � �    �
" 
 3 
 ��           "    1� �   ��    � %               o%   o           %               
" 
 3 
 ��           �"    1� �   �� !  	 � %               o%   o           � �    �
" 
 3 
 ��           #    1� �   �� !  	 � %               o%   o           � �    �
" 
 3 
 ��           �#    1� �   �� !  	 � %               o%   o           � �    �
" 
 3 
 ��           �#    1� �   �� !  	 � %               o%   o           o%   o           
" 
 3 
 ��           p$    1� �   �� !  	 � %               o%   o           � �    �
" 
 3 
 ��           �$    1�    �� !  	 � %               o%   o           � �    �
" 
 3 
 ��           X%    1�   	 �� �   � %               o%   o           %               
" 
 3 
 {�           �%    1� '   {� �   � %               o%   o           %               
" 
 3 
 {�           P&    1� 0   {�    � %               o%   o           o%   o           
" 
 3 
 ��           �&    1� A   ��    � %               o%   o           o%   o           
" 
 3 
 ��           H'    1� P   ��    � %               o%   o           %               
" 
 3 
 ��           �'    1� ^   ��    � %               o%   o           %               
" 
 3 
 ��           @(    1� o   ��    � %               o%   o           %               
" 
 3 
 ��           �(    1� �   �� �   � %               o%   o           %       
       
" 
 3 
 ��           8)    1� �   �� �   � %               o%   o           o%   o           
" 
 3 
 ��           �)    1� �   �� �   � %               o%   o           %              
" 
 3 
 ��           0*    1� �   �� �   � %               o%   o           o%   o           
" 
 3 
 ��           �*    1� �   �� �   � %               o%   o           %              
" 
 3 
 ��           (+    1� �   �� �   � %               o%   o           o%   o           
" 
 3 
 ��           �+    1� �   �� �   � %               o%   o           %              
" 
 3 
 ��            ,    1� �   �� �   � %               o%   o           o%   o           
" 
 3 
 �           �,    1� �   � !  	 � %               o%   o           � �    �P �L 
�H T   %              �     }        �GG %              
" 
 3 
 ��           d-    1� �   �� �   � %               o%   o           %               
" 
 3 
 ��           �-    1�    �� �   � %               o%   o           o%   o           
" 
 3 
 ��           \.    1�    �� �   � %               o%   o           � �    �
" 
 3 
 ��           �.    1�     �� �   � %               o%   o           � 6  - �
" 
 3 
 ��           D/    1� d   �� �   � %               o%   o           � �    �
" 
 3 
 ��           �/    1� {   �� �   � %               o%   o           � �   �
" 
 3 
 � �          ,0    1� �   � � 6     
" 
 3 
 ��           h0    1� �   �� �   � %               o%   o           � �    �
" 
 3 
 � �          �0    1� �  
 � � 6     
" 
 3 
 � �          1    1� �   � � 6     
" 
 3 
 ��           T1    1� �   �� !  	 � %               o%   o           � �    �
" 
 3 
 ��           �1    1� �   �� �   � %               o%   o           � �    �
" 
 3 
 ��           <2    1�     �� 6   � %               o%   o           o%   o           
" 
 3 
 ��           �2    1�     �� �   � %               o%   o           � %   ! �
" 
 3 
 �           ,3    1� G    � �   � %               o%   o           � �    �
" 
 3 
 ��           �3    1� T    �� �   � %               o%   o           � g    
" 
 3 
 ��           4    1� v   	 �� �   � %               o%   o           o%   o           
" 
 3 
 ��           �4    1� �    ��    � %               o%   o           %               
" 
 3 
 � �          5    1� �    � � 6     
" 
 3 
 ��           H5    1� �    �� �   � %               o%   o           � �    �
" 
 3 
 ��           �5    1� �    �� !  	 � %               o%   o           � �    �
" 
 3 
 ��           06    1� �    �� !  	 � %               o%   o           � �    �
" 
 3 
 � �          �6    1� �    � � 6     
" 
 3 
 � �          �6    1� �    � � !  	   
" 
 3 
 {�           7    1� �    {�    � o%   o           o%   o           %               
" 
 3 
 � �          �7    1� !   � �      
" 
 3 
 � �          �7    1� -!   � � !  	   
" 
 3 
 � �          8    1� ;!   � � !  	   
" 
 3 
 � �          L8    1� N!   � � !  	   
" 
 3 
 � �          �8    1� _!   � � !  	   
" 
 3 
 � �          �8    1� p!   � � !  	   
" 
 3 
 � �           9    1� �!   � � 6     
" 
 3 
 ��           <9    1� �!   �� �   � %               o%   o           � �!  4 �
" 
 3 
 � �          �9    1� �!   � � 6     
" 
 3 
 � �          �9    1� �!   � � 6     
" 
 3 
 � �          (:    1� �!   � � 6     
" 
 3 
 � �          d:    1� "   � � !  	   
" 
 3 
 � �          �:    1� "   � � !  	   
" 
 3 
 � �          �:    1� ."   � � !  	   
" 
 3 
 � �          ;    1� @"   � �      
" 
 3 
 ��           T;    1� M"   �� !  	 � %               o%   o           � �    �
" 
 3 
 ��           �;    1� ["   �� !  	 � %               o%   o           � �    �
" 
 3 
 ��           <<    1� g"   �� !  	 � %               o%   o           � �    �
" 
 3 
 ��           �<    1� |"   �� !  	 � %               o%   o           � �    �
" 
 3 
 ��           $=    1� �"   ��    � %               o%   o           %               
" 
 3 
 ��           �=    1� �"   ��    � %               o%   o           o%   o           
" 
 3 
 ��           >    1� �"   ��    � %               o%   o           %               
" 
 3 
 ��           �>    1� �"   ��    � %               o%   o           %               
" 
 3 
 ��           ?    1� �"   ��    � %               o%   o           o%   o           
" 
 3 
 ��           �?    1� �"   ��    � %               o%   o           %               
" 
 3 
 � �          @    1� �"   � � !  	   
" 
 3 
 ��           H@    1� #   ��    � %               o%   o           %              
" 
 3 
 � �          �@    1� #   � � !  	   
" 
 3 
 � �           A    1� !#   � � !  	   
" 
 3 
 � �          <A    1� 0#  
 � � !  	   
" 
 3 
 {�           xA    1� ;#   {� !  	 � %               o%   o           � �"   �
" 
 3 
 ��           �A    1� M#   �� !  	 � %               o%   o           � �    {
" 
  
    " 
 5  � %     start-super-proc z� %     adm2/smart.p ��P �L 
�H T   %              �     }        �GG %              
" 
 3 
   �       C    6� �     
" 
 3 
   
�        8C    8
" 
 4 
   �        XC    ��     }        �G 4              
" 
 4 
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
" 
 2 
 �
" 
 2 
 � 
" 
 2 
 �
" 
 2 
   (�  L ( l       �        �D    �� �   � P   �        �D    �@    
� @  , 
�       �D    �� �   �p�               �L
�    %              � 8      �D    � $         � �          
�    � �   �
" 
 2 
 �p� @  , 
�       �E    �� =   �p�               �L" 
 8  , �   � �#   �� �#   � �     }        �A      |    " 
 8    � �#   �%              (<   \ (    |    �     }        �A� �#   �A" 
 9  �    " 
 8  �" 
 9  �  < " 
 8  �" 
 9  �(    |    �     }        �A� �#   �A" 
 9  �
�H T   %              �     }        �GG %              
" 
 2 
 �
" 
 2 
 � 
" 
 2 
 �
" 
 2 
   (�  L ( l       �        �G    �� �   � P   �        �G    �@    
� @  , 
�       �G    �� �   �p�               �L
�    %              � 8      �G    � $         � �          
�    � �   �
" 
 2 
 �p� @  , 
�       �H    �� �  
 �p�               �L" 
 8  , 
�H T   %              �     }        �GG %              
" 
 2 
 �
" 
 2 
 � 
" 
 2 
 �
" 
 2 
   (�  L ( l       �        �I    �� �   � P   �        �I    �@    
� @  , 
�       �I    �� �   �p�               �L
�    %              � 8      �I    � $         � �          
�    � �   �
" 
 2 
 �p� @  , 
�       �J    �� &   �p�               �L
" 
  
 , 
�H T   %              �     }        �GG %              
" 
 2 
   
" 
 2 
 �
" 
 2 
   
" 
 2 
   (�  L ( l       �        XK    �� �   � P   �        dK    �@    
� @  , 
�       pK    �� �     p�               �L
�    %              � 8      |K    � $         � �          
�    � �     
" 
 2 
 �p� @  , 
�       �L    �� �  
 �p�               �L%     SmartWindow 
" 
 2 
   p� @  , 
�       �L    �� �     p�               �L%      WINDOW  
" 
 2 
  p� @  , 
�       PM    �� �    p�               �L%               
" 
 2 
  p� @  , 
�       �M    �� m    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
" !  
 � (   � 
" !  
 �    �        �N    �� �   �
" !  
   � 8      �N    � $         � �          
�    � �   �
" !  
   �        4O    �
" !  
   �       TO    /
" !  
   
" !  
   �       �O    6� �     
" !  
   
�        �O    8
" !  
   �        �O    �
" !  
   �       �O    �
" !  
   p�    � �#   �
�    �     }        �G 4              
" !  
 ߱G %              G %              
�     }        �
" "  
    (   � 
" "  
 �    �        �P    �A" "   �A
" "  
   
�        �P    �@ � 
" "  
 �" "     �       }        �
" "  
 � %              %                " 
 5  � %     start-super-proc y� %     adm2/appserver.p ���    � 8$     
�    �     }        �%               %      Server  - �     }        �    " 
 :  �� �    � %                   " 
 ;  �� �    � %      NONE    p�,  8         $     " 
 ;  �        � R$   �
�    
�H T   %              �     }        �GG %              
" 
 2 
 �
" 
 2 
 � 
" 
 2 
 �
" 
 2 
   (�  L ( l       �        <S    �� �   � P   �        HS    �@    
� @  , 
�       TS    �� �   �p�               �L
�    %              � 8      `S    � $         � �          
�    � �   �
" 
 2 
 �p� @  , 
�       pT    �� 2   �p�               �L" 
 <  , p�,  8         $     " 
 :  �        � `$   �
�     " 
 5  � %     start-super-proc y� %     adm2/visual.p ��   � �     � �$     � �$  y   
�H T   %              �     }        �GG %              
" 
 2 
 �
" 
 2 
 � 
" 
 2 
 �
" 
 2 
   (�  L ( l       �        �U    �� �   � P   �        �U    �@    
� @  , 
�       �U    �� �   �p�               �L
�    %              � 8      �U    � $         � �          
�    � �   �
" 
 2 
 �p� @  , 
�        W    �� �   �p�               �L" 
 =  , � 
"    
 � %     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP ��%     processAction   
�    %     CTRL-PAGE-DOWN  " 
 5  � %     start-super-proc x� %     adm2/containr.p %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    � F%   �
�    � X%   � A    �    � F%     
�    � d%   � %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    � F%   � 
�    � �%   �%     modifyListProperty  
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
"    
 � 
" 
  
 � %     contextHelp 
"    
   
�    
�    %               
�H T   %              �     }        �GG %              
" 
 2 
 �
" 
 2 
 � 
" 
 2 
 �
" 
 2 
 �(�  L ( l       �        L[    �� �   � P   �        X[    �@    
� @  , 
�       d[    �� �   �p�               �L
�    %              � 8      p[    � $         � �   �     
�    � �   � 
" 
 2 
 �p� @  , 
�       �\    �� �    �p�               �L
�             �G
�H T   %              �     }        �GG %              
" 
 2 
 �
" 
 2 
 � 
" 
 2 
 �
" 
 2 
 �(�  L ( l       �        ,]    �� �   � P   �        8]    �@    
� @  , 
�       D]    �� �   �p�               �L
�    %              � 8      P]    � $         � �   �     
�    � �   �
" 
 2 
 �p� @  , 
�       `^    �� �"   �p�               �L%              (        �     }        �G� A	   �G� 
" 
  
 �
" 
  
   �         _    �%              
" 
  
 � 
" 
  
 � �     }        �%               
" 
  
 � %      CLOSE   %               %      CLOSE   H    4             " 
 +    " 
 ,  �" 
 -  �" 
 .  �%               � �%  !   %               %$     lib/tt-file-to-text-7zip ��"      "      "          "    �� �%    � %               �    }        �� �%     �     }        �� �%     %     Carga-Temporal  %      Excel-2 �     }        �� �%     �    }        �� �%       *    � �%     %               �X    D    0    ,    %              %       '      � &     � 	&   �� 	&   �"    � .(      �     }        �"    ��    }        �� �%     �     }        �� �%     %     lib/tt-file 
�             �G"      "      �     }        �� �%     �    }        �� �%               "      � &   �%               �   � &     � &     "               "      � &   �%               �   � &     � &     "       4               � &  ) ߱"    � � D&     "    �"          < "    �%              � F&     %      
       � f&     %               "               "      � &   �%               �   � &     � &     "               "      � &   �%               �   � &     � &     "       P     4               � w&     "    �� D&   �z     "    � z     "    �"      "      � �%    �� �%      " 
 $  �" 
 %  �� }&     " 
 $    � �&     � �&     � �&         " #   �%              �            B" 
 $    " 
 %    � �&     � �&     � �&         " $   �%              �            B" 
 %    �           �            F�           � �     �            B� �         �     }        B� �   B%              � �&     T   &    �     }        B&    &    &    � 4                     " %     � �&     " %         �     }        B� �%    B%               �     }        B&    &     * &   � '     �     }        B� �%      %               �       	     B" &     %     lkup/c-client   � %'         "    �%              �     }        B"          �     }        B� �%    B%               �     }        B&    &     * '   � N'     �     }        B� �%      %               �            B" '         �     }        B� �%    B%               �     }        B&    &     * (   � �'     �     }        B� �%      %               �            B" (     %     lkup/c-provee   � �'         "    �%              �     }        B"          �     }        B� �%    B%               �     }        B&    &     * )   � �'     �     }        B� �%      %               �            B" )     �              �             �              �                 #   r�Cliente  %              �       
      %              �       
      %               �            B� �%      � �'     �              �             �              �             �              �             �              �             �              �             �              �             �              �             � �'     �              �             X X   ( (       " 
 +  %                  " 
 -    %              ( (       " 
 0    %                  " 
 1  �%              �            B� �%      �             %               �             %              ( (       " 
 0  {%                  " 
 1    %              �            B� �%      �             %               �             %               �            B� �%      �             %              �             %              � �'     � 
" 
  
 � 
" 
  
 {
" 
  
 ��        \q    %%              
�     }        �
" 
  
   %     destroyObject       �     }        �    �  � �'  	   %               
" 
  
 � 
�    %     createObjects    �     }        �%     initializeObject x�  �     }        �    < " 
 %  �%              " 
 %    � �%        "      &     *    " 
      "    &    &     * '   "    &    &     *    "      � �%          " 
   �� �%    � "                 " 
     � (     "          < " 
 $  �%              " 
 $    " *   &    &     *    " *     � �%    &    &     *    " 
     "    &    &     * &   �    }        �� �%     � �%    �� �%    �� �%      � �%    �� �%      � �%    �� �%    �%               %       �       $   " 
 ,  �  8    " 
   �� �   � T   %              " 
     � �&   �" 
 +    " 
     " 
 -    " 
       $   " 
 -  �  8    " 
   �� �   � T   %              " 
     � �&   � $   " 
 -  �  8    " 
   �� �   � T   %              " 
     � �&   �" 
 -    " 
 !    " 
 .    " 
 "    %      Carga-Lista-Articulos �%      Carga-Lista-Clientes ��  (       " 
   �� �   �     " 
 ,  �%                   %              %                   " +     %                  " +     �     "      �     "          " ,   �� �%    � T    " +     "                  " ,     � (     T    " +     "    �" 
         %              %               " 
   �" 
 *        %              %                   " +     %                  " +     �     " ,     �     " ,     T    %              T    " +   � " ,     � �&   �( �       " 
 ,    %              X (   ( (       " 
 +    %                   " 
 -  �%                   " 
 .    %               %      Resumen-por-division ��%              X (   ( (       " 
 +  �%                  " 
 -    %                   " 
 .    %               %     Resumen-por-cliente %              X (   ( (       " 
 +  �%                  " 
 -    %                  " 
 .    %               ( (       " 
 0  �%                  " 
 1    %              %     Resumen-por-climat  %              X (   ( (       " 
 +  �%                   " 
 -    %                  " 
 .    %               ( (       " 
 0  �%                  " 
 1    %              %     Resumen-por-resmat  %      Resumen-por-producto ��%              X (   ( (       " 
 +  �%                   " 
 -    %                  " 
 .    %              ( (       " 
 0  �%                  " 
 1    %              %     Resumen-por-resmat  %              X (   ( (       " 
 +  �%                   " 
 -    %                   " 
 .    %              %      Resumen-por-vendedor ��%              X (   ( (       " 
 +  �%                  " 
 -    %                   " 
 .    %              %     Resumen-por-vendcli %                  " +   �%               %     Resumen-General %     src/bin/_dateif  " 
 *  c/     " 
 *  at%              " 
    " 
        , %              %                   " 
   �%                  %              %                   " +     %                  " +     �     " ,     �     " ,     T    %              T    " +   � " ,     � �&   �( �       " 
 ,    %              X (   ( (       " 
 +    %                   " 
 -  �%                   " 
 .    %               %      Resumen-por-division ��%              X (   ( (       " 
 +  �%                  " 
 -    %                   " 
 .    %               %     Resumen-por-cliente %              X (   ( (       " 
 +  �%                  " 
 -    %                  " 
 .    %               ( (       " 
 0  �%                  " 
 1    %              %     Resumen-por-climat  %              X (   ( (       " 
 +  �%                   " 
 -    %                  " 
 .    %               ( (       " 
 0  �%                  " 
 1    %              %     Resumen-por-resmat  %      Resumen-por-producto ��%              X (   ( (       " 
 +  �%                   " 
 -    %                  " 
 .    %              ( (       " 
 0  %                  " 
 1    %              %     Resumen-por-resmat  %              X (   ( (       " 
 +  �%                   " 
 -    %                   " 
 .    %              %      Resumen-por-vendedor ��%              X (   ( (       " 
 +  �%                  " 
 -    %                   " 
 .    %              %     Resumen-por-vendcli %                  " +   �%               %     Resumen-General �    }        �� �%      "     � "     � &    &    &    &        %              %              * -   � �%        "      &    %              � �&     "       "       T   &    "      &    &    &    &    &    &    &    0        %              %              %               * -   %               %               %               %               %               %               %               %               %               %               %               %               %               %               %               %                   " 
   �%              %     Resumen-por-familia � �(  
 �    " 
 ,  �%               �      l   "    �(L   8       "    � "    � %              � �(   � � �%      � (   �� �(   �    " 
 +  �%                  " 
 /  �%                �      l   "    �(L   8       "    � "    � %              � �(   � � �%      � (   �� �(   � �      l   "    �(L   8       "    � "    � %              � �(   � � �%      � (   �� �(  * �    " 
 -  �%              ( (       " 
 0    %                   " 
 1  �%                �      l   "    �(L   8       "    � "    � %              � �(   � � �%      � (   �� �(  $ �    " 
 0  �%               �      l   "    �(L   8       "    � "    � %              � �(   � � �%      � (   �� )   �    " 
 1  �%               �      l   "    �(L   8       "    � "    � %              � �(   � � �%      � (   �� )   � �      l   "    �(L   8       "    � "    � %              � �(   � � �%      � (   �� %)   �    " 
 .  �%               �      l   "    �(L   8       "    � "    � %              � �(   � � �%      � (   �� 8)   � ,         "    �G %              "    �( X       " 
 -    %              ( (       " 
 0  �%                   " 
 1    %                    "      � A)  4 �     "      � v)  H �(        �     }        �G� A	   �G� 
" 
  
 �
" 
  
   �     }        �
�    
" 
  
 �" 
 &  �" 
   �" 
 *    " 
 ,    " 
     " 
 +    " 
     " 
 '    " 
 $    " 
 /    " 
 -  � " 
   � " 
 0  � " 
   � " 
 1  � " 
    � " 
 #    " 
 %    " 
 !  � " 
 (    " 
 .    " 
 "    " 
 )    
" 
  
 �
" 
  
   �            B� *     %                  " 
 ,  �%              T   " .     "      � *     " .          " .     %              T   " .     "      � *     " .          " .     %                  " 
 +  �%                  " 
 /  �%               T   " .     "      � *     " .          " .     %              T   " .     "      � *     " .          " .     %              T   " .     "      � *     " .          " .     %              T   " .     "      � *     " .          " .     %              T   " .     "      � *     " .          " .     %              T   " .     "      � *     " .          " .     %              T   " .     "      � *     " .          " .     %              T   " .     "      � *     " .          " .     %                  " 
 -  �%              ( (       " 
 0    %                   " 
 1  �%               T   " .     "      � *     " .          " .     %              T   " .     "      � *     " .          " .     %              T   " .     "      � *     " .          " .     %              T   " .     "      � *     " .          " .     %              T   " .     "      � *     " .          " .     %                  " 
 0  �%              T   " .     "      � *     " .          " .     %              T   " .     "      � *     " .          " .     %                  " 
 1  �%              T   " .     "      � *     " .          " .     %              T   " .     "      � *     " .          " .     %              T   " .     "      � *     " .          " .     %                  " 
 .  �%              T   " .     "      � *     " .          " .     %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              4     	    %              4         %              4     	    %              4         %              4     	    %              4         %              4     	    %              %      CLOSE   %                        +   +  %              +  � �   �"     � "     � &    &    &    &        %              %              * /   "     � "     � &    &    &    &        %              %              " /     &    &       4        "      � (                " 0     � �&     " 0        4        "      � (                " 0     � �&     " 0     �           "      �           %              (        " 1     " 2     %               %                  " 3     %              � 4                     " 1     � �&     " 1     %      SUPER   %              � �%           " 
     %                  " 
   �� �%    � "                 " 
     � (     "          " 
   �%       �      � �%    � %               � �%      %               � �%      � �%      " 
     " 
         " 
     &    " 
        " 
     &    " 
        " 
     &    " 
        " 
     &    " 
    &    &    � ,   � ,   t <   0 <       %              %               ,   & 	       S    " 4     & 
   &     ,   &        S    " 4     &    &        &        " 4     &        &        " 4     &    " 4   � &    &    " 4     &    &    " 4   � &    &    � �%    � � �%    � � �%    � " 4   �     " 
     &    " 
        " 
     &    " 
        " 
 	    &    " 
 	   &    &    � ,   H D    ,   %                  &        " '     &     4   &    $    4    '     %              &        & 	       " '     & 
   " '     &    &    " '     " '     &    &    &    &        %              %              ( <       " 
   �%                    '    " 
   �" 
   �%               �            B     �     �     �     �     � $    t     `     L       $        � �*     � �*          " 5     � �*     � D&     " 5     � D&          " 5     � &     � �*  
   " 4     � �*  	   " 4     � �*          " 
     %              %               %              %               %              %               %              %               %              %               %              %               %              %               %              � �%      " 4               "      " 
   �    "    " 
 *  �  $    4         %              " 4 
    %               $    4     	  
 %              " 4     %               $    4     
   %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %              T      @   "          %              %               " 
         "    � " 
 *     $    4         %              " 4 
    %               $    4     	  
 %              " 4     %               $    4     
   %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %              h h    T   "         ,  " 
      " 
          " 
     %               T   "    �   ,  " 
 *     " 
 *  �     " 
 *  �%               $    4         %              " 4 
    %               $    4     	  
 %              " 4     %               $    4     
   %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %              p h    \   "         , %              %                   " 
   �%               T   "         ,  " 
 *  � " 
 *  �     " 
 *  � %               $    4         %              " 4 
    %               $    4     	  
 %              " 4     %               $    4     
   %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %              " 
 ,        "    �� �%    � " 4          "      " 4      4               "    ߱� �&     " 0     � *         " 
 +  �%              " &     " & 
    " &     " &     " &     " & 	    " &     � �%          " 
 /  �%                   "    �� �%    � " 4          "      " 4      4               "    ߱� �&     " &     � *                " &     � �&     z     " &                " & 
    � �&     z     " &     " &     &    &    * 6              " &     � �&     z     " 6     " &     " &   � " &   � " & 	  � &    &    &    &    &    &    0        %              %              %              * 7              " &     � �&     z     " 7                " &    � �&     z     " 7                " & 	   � �&     z     " 7 	               " 7   	 � �&     z     " 7         "    �� �%    �      " 
     � *                "      " 
     � *   �           "      " 
     � *   �           "      " 
     � *   �           "      " 
     � *   �           "      " 
     � *   �           "      " 
     � *   �           "      " 
     � *   �    " 
 -  �%              ( (       " 
 0    %                   " 
 1  �%                   "    �� �%    � z     " 4          "      z     " 4      4               "    ߱� �&     " '     � *      H     4               "      " '     � �&     " 1     � *      H     4               "      " '     � �&     " %     � *                "      " '     � *                "      " '     � *         " 
 0  �%                  "    �� �%    �  4               " '   ߱� �&     " 1     � *      H     4               "      " '     � �&     " 1     � *      H     4               "      " '     � �&     " %     � *         " 
 1  �%                  "    �� �%    �      " '     � *                "      " '     � *     " ' 	    &    &    * 8    H     4               "      " ' 	    � �&     " 8     � *                "      " ' 	    � *     4  '     %              &    &    * (    `     L     8      $   "      4    '     %              � �&     " (     � *      8      $   "      4  ߱'     %              � *         " 
 .  �%                  "    �� �%    �  4               " 4   ߱� �&     " 9     � *      H     4               "      " 4     � �&     " 9     � *     "    �&    &     *    "       $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %              %               � �%          " 
   �%               � �%    � %               � �%      %               � �%      � �%      " 
     " 
         " 
     &    " 
        " 
     &    " 
        " 
     &    " 
        " 
     &    " 
    &    &    � ,   � ,   t <   0 <       %              %               ,   & 	       S    " 4     & 
   &     ,   &        S    " 4     &    &        &        " 4     &        &        " 4     &    " 4   � &    &    " 4     &    &    " 4   � &    &    � �%    � � �%    � � �%    � " 4   �     " 
     &    " 
        " 
     &    " 
        " 
 	    &    " 
 	   &    &    � ,   H D    ,   %                  &        " '     &     4   &    $    4    '     %              &        & 	       " '     & 
   " '     &    &    " '     " '     &    &    &    &        %              %              ( <       " 
   �%                    '    " 
   �" 
   �%               �            B     �     �     �     �     � $    t     `     L       $        � �*     � �*          " 5     � �*     � D&     " 5     � D&          " 5     � &     � �*  
   " 4     � �*  	   " 4     � �*          " 
     %              %               %              %               %              %               %              %               %              %               %              %               %              %               %              � �%      " 4               "      " 
   �    "    �" 
 *  �  $    4         %              " 4 
    %               $    4     	  
 %              " 4     %               $    4     
   %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %              T      @   "          %              %               " 
         "    � " 
 *     $    4         %              " 4 
    %               $    4     	  
 %              " 4     %               $    4     
   %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %              h h    T   "         ,  " 
      " 
          " 
     %               T   "    �   ,  " 
 *     " 
 *  �     " 
 *  �%               $    4         %              " 4 
    %               $    4     	  
 %              " 4     %               $    4     
   %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %              p h    \   "         , %              %                   " 
   q%               T   "         ,  " 
 *  � " 
 *  �     " 
 *  � %               $    4         %              " 4 
    %               $    4     	  
 %              " 4     %               $    4     
   %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %               $    4        %              " 4     %              " 
 ,        "    �� �%    � " 4          "      " 4      4               "    ߱� �&     " 0     � *         " 
 +  �%              " &     " & 
    " &     " &     " &     " & 	    " &     � �%          " 
 /  �%                   "    �� �%    � " 4          "      " 4      4               "    ߱� �&     " &     � *                " &     � �&     z     " &                " & 
    � �&     z     " &     " &     &    &    * 6              " &     � �&     z     " 6     " &     " &   � " &   � " & 	  � &    &    &    &    &    &    0        %              %              %              * 7              " &     � �&     z     " 7                " &    � �&     z     " 7                " & 	   � �&     z     " 7 	               " 7   	 � �&     z     " 7         "    � �%    �      " 
     � *                "      " 
     � *              "      " 
     � *              "      " 
     � *              "      " 
     � *              "      " 
     � *              "      " 
     � *              "      " 
     � *       " 
 -  %              ( (       " 
 0    %                   " 
 1  %                   "    � �%    � z     " 4          "      z     " 4      4               "    ߱� �&     " '     � *      H     4               "      " '     � �&     " 1     � *      H     4               "      " '     � �&     " %     � *                "      " '     � *                "      " '     � *         " 
 0  �%                  "    �� �%    �  4               " '   ߱� �&     " 1     � *      H     4               "      " '     � �&     " 1     � *      H     4               "      " '     � �&     " %     � *         " 
 1  %                  "    � �%    �      " '     � *                "      " '     � *     " ' 	    &    &    * 8    H     4               "      " ' 	    � �&     " 8     � *                "      " ' 	    � *     4  '     %              &    &    * (    `     L     8      $   "      4    '     %              � �&     " (     � *      8      $   "      4  ߱'     %              � *         " 
 .  �%                  "    �� �%    �  4               " 4   ߱� �&     " 9     � *      H     4               "      " 4     � �&     " 9     � *     "    �&    &     *    "       $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %              %              � �%           " 
     %                  " 
   �� �%    � "                 " 
     � (     "          " 
   �%       �      � �%    � %               � �%      " 
     " 
         " 
     &    " 
        " 
     &    " 
    &    &    t ,   0 <       %              %               ,   &        S    " :     &    &        &        " :     & 	   " :   � &    &    " :     &    &    " :   � &    &    ( <       " 
   �%                    '    " 
   �" 
   �%               �            B     �     �     �     �     � $    t     `     L       $        � �+     � �*          " 5     � �*     � D&     " 5     � D&          " 5     � &     � �*  
   " :     � �*  	   " :     � �*          " 
     %              %               %              %               %              %               %              %               %              %               %              %               %              %               %              � �%      " :               "      " 
   �    "    �" 
 *  �  $    4     	    %              " :     %               $    4     
   %              " :     %               $    4        %              " :     %               $    4        %              " : 
    %               $    4       
 %              " :     %               $    4        %              " :     %              T      @   "          %              %               " 
         "    � " 
 *     $    4     	    %              " :     %               $    4     
   %              " :     %               $    4        %              " :     %               $    4        %              " : 
    %               $    4       
 %              " :     %               $    4        %              " :     %              h h    T   "         ,  " 
      " 
          " 
     %               T   "    [   ,  " 
 *     " 
 *  [     " 
 *  �%               $    4     	    %              " :     %               $    4     
   %              " :     %               $    4        %              " :     %               $    4        %              " : 
    %               $    4       
 %              " :     %               $    4        %              " :     %              p h    \   "         , %              %                   " 
   �%               T   "         ,  " 
 *  � " 
 *  �     " 
 *  � %               $    4     	    %              " :     %               $    4     
   %              " :     %               $    4        %              " :     %               $    4        %              " : 
    %               $    4       
 %              " :     %               $    4        %              " :     %              " 
 ,        "    Z� �%    � " :          "      " :      4               "    ߱� �&     " 0     � *      H     4               "      " 0     � �&     " 0     � *         " 
 +  q%              " &     " & 
    " &     " &     " &     " & 	    " &     � �%          " 
 /  q%                   "    q� �%    � " :          "      " :      4               "    ߱� �&     " &     � *                " &     � �&     z     " &                " & 
    � �&     z     " &     " &     &    &    * 6              " &     � �&     z     " 6     " &     " &   � " &   � " & 	  � &    &    &    &    &    &    0        %              %              %              * 7              " &     � �&     z     " 7                " &    � �&     z     " 7                " & 	   � �&     z     " 7 	               " 7   	 � �&     z     " 7         "    � �%    �      " 
     � *                "      " 
     � *   q           "      " 
     � *   q           "      " 
     � *   q           "      " 
     � *   q           "      " 
     � *   q           "      " 
     � *   q           "      " 
     � *   q"    �&    &     *    "       $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %              %               � �%          " 
   q%               � �%    � %               � �%      " 
     " 
         " 
     &    " 
        " 
     &    " 
    &    &    t ,   0 <       %              %               ,   &        S    " :     &    &        &        " :     & 	   " :   � &    &    " :     &    &    " :   � &    &    ( <       " 
   %                    '    " 
   " 
   �%               �            B     �     �     �     �     � $    t     `     L       $        � �+     � �*          " 5     � �*     � D&     " 5     � D&          " 5     � &     � �*  
   " :     � �*  	   " :     � �*          " 
     %              %               %              %               %              %               %              %               %              %               %              %               %              %               %              � �%      " :               "      " 
   �    "    q" 
 *  �  $    4     	    %              " :     %               $    4     
   %              " :     %               $    4        %              " :     %               $    4        %              " : 
    %               $    4       
 %              " :     %               $    4        %              " :     %              T      @   "          %              %               " 
         "    � " 
 *     $    4     	    %              " :     %               $    4     
   %              " :     %               $    4        %              " :     %               $    4        %              " : 
    %               $    4       
 %              " :     %               $    4        %              " :     %              h h    T   "         ,  " 
      " 
          " 
     %               T   "    �   ,  " 
 *     " 
 *  q     " 
 *  �%               $    4     	    %              " :     %               $    4     
   %              " :     %               $    4        %              " :     %               $    4        %              " : 
    %               $    4       
 %              " :     %               $    4        %              " :     %              p h    \   "         , %              %                   " 
   �%               T   "         ,  " 
 *  � " 
 *  �     " 
 *  � %               $    4     	    %              " :     %               $    4     
   %              " :     %               $    4        %              " :     %               $    4        %              " : 
    %               $    4       
 %              " :     %               $    4        %              " :     %              " 
 ,        "    �� �%    � " :          "      " :      4               "    ߱� �&     " 0     � *      H     4               "      " 0     � �&     " 0     � *         " 
 +  �%              " &     " & 
    " &     " &     " &     " & 	    " &     � �%          " 
 /  �%                   "    �� �%    � " :          "      " :      4               "    ߱� �&     " &     � *                " &     � �&     z     " &                " & 
    � �&     z     " &     " &     &    &    * 6              " &     � �&     z     " 6     " &     " &   � " &   � " & 	  � &    &    &    &    &    &    0        %              %              %              * 7              " &     � �&     z     " 7                " &    � �&     z     " 7                " & 	   � �&     z     " 7 	               " 7   	 � �&     z     " 7         "    Z� �%    �      " 
     � *                "      " 
     � *   Z           "      " 
     � *   Z           "      " 
     � *   Z           "      " 
     � *   Z           "      " 
     � *   Z           "      " 
     � *   Z           "      " 
     � *   Z"    �&    &     *    "       $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %              %              � �%           " 
     %                  " 
   Z� �%    � "                 " 
     � (     "          " 
   Z%       �      � �%    � %               � �%      " 
     " 
     " 
     " 
         " 
     &    " 
        " 
 	    &    " 
 	   &    &    � ,   x <   T    0        %              %              8    " ;     &    8    " ;     &     ,   &        S    " ;     & 	   &        & 
       " ;     &    " ;   � &    &    " ;     &    &    " ;     &    &    " ;     " ;     &    &    &    &        %              %              " ;   � &    &    ( <       " 
   q%                    '    " 
   q" 
   �%               �            B     �     �     �     �     � $    t     `     L       $        � �+     � �*          " 5     � �*     � D&     " 5     � D&          " 5     � &     � �*  
   " ;     � �*  	   " ;     � �*          " 
     %              %               %              %               %              %               %              %               %              %               %              %               %              %               %              � �%      " ;               "      " 
   �    "    q" 
 *  �  $    4     	    %              " ;     %               $    4     
   %              " ; 
    %               $    4       
 %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %              T      @   "          %              %               " 
         "    � " 
 *     $    4     	    %              " ;     %               $    4     
   %              " ; 
    %               $    4       
 %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %              h h    T   "         ,  " 
      " 
          " 
     %               T   "    �   ,  " 
 *     " 
 *  �     " 
 *  �%               $    4     	    %              " ;     %               $    4     
   %              " ; 
    %               $    4       
 %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %              p h    \   "         , %              %                   " 
   �%               T   "         ,  " 
 *  � " 
 *  �     " 
 *  � %               $    4     	    %              " ;     %               $    4     
   %              " ; 
    %               $    4       
 %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %              " 
 ,        "    � �%    � " ;          "      " ;      4               "    ߱� �&     " 0     � *      H     4               "      " 0     � �&     " 0     � *         " 
 +  %              " &     " & 
    " &     " &     " &     " & 	    " &     � �%          " 
 /  %                   "    � �%    � " ;          "      " ;      4               "    ߱� �&     " &     � *                " &     � �&     z     " &                " & 
    � �&     z     " &     " &     &    &    * &              " &     � �&     z     " 6     " &     " &   � " &   � " & 	  � &    &    &    &    &    &    0        %              %              %              * 7              " &     � �&     z     " 7                " &    � �&     z     " 7                " & 	   � �&     z     " 7 	               " 7   	 � �&     z     " 7         "    �� �%    �      " 
     � *                "      " 
     � *   �           "      " 
     � *   �           "      " 
     � *   �           "      " 
     � *   �           "      " 
     � *   �           "      " 
     � *   �           "      " 
     � *   �    " 
 -  �%                  " 
 0  �%                  "    �� �%    � " ;          "      " ;      4               "    ߱� �&     " 1     � *      H     4               "      " ;     � �&     " %     � *         " 
 1  �%                  "    �� �%    �      " ;     � *                "      " ;     � *          "      " ;     "     � " ;   � &    &    &    &        %              %              * <    4               "    ߱� �&     " <     � *          "      � *                "      � �%      � *   Z"    �&    &     *    "       $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %              %               � �%          " 
   �%               � �%    � %               � �%      " 
     " 
     " 
     " 
         " 
     &    " 
        " 
 	    &    " 
 	   &    &    � ,   x <   T    0        %              %              8    " ;     &    8    " ;     &     ,   &        S    " ;     & 	   &        & 
       " ;     &    " ;   � &    &    " ;     &    &    " ;     &    &    " ;     " ;     &    &    &    &        %              %              " ;   � &    &    ( <       " 
   �%                    '    " 
   �" 
   �%               �            B     �     �     �     �     � $    t     `     L       $        � �+     � �*          " 5     � �*     � D&     " 5     � D&          " 5     � &     � �*  
   " ;     � �*  	   " ;     � �*          " 
     %              %               %              %               %              %               %              %               %              %               %              %               %              %               %              � �%      " ;               "      " 
   �    "    q" 
 *  �  $    4     	    %              " ;     %               $    4     
   %              " ; 
    %               $    4       
 %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %              T      @   "          %              %               " 
         "    � " 
 *     $    4     	    %              " ;     %               $    4     
   %              " ; 
    %               $    4       
 %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %              h h    T   "         ,  " 
      " 
          " 
     %               T   "    q   ,  " 
 *     " 
 *  q     " 
 *  �%               $    4     	    %              " ;     %               $    4     
   %              " ; 
    %               $    4       
 %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %              p h    \   "         , %              %                   " 
   Z%               T   "         ,  " 
 *  � " 
 *  �     " 
 *  � %               $    4     	    %              " ;     %               $    4     
   %              " ; 
    %               $    4       
 %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %               $    4        %              " ;     %              " 
 ,        "    q� �%    � " ;          "      " ;      4               "    ߱� �&     " 0     � *      H     4               "      " 0     � �&     " 0     � *         " 
 +  �%              " &     " & 
    " &     " &     " &     " & 	    " &     � �%          " 
 /  �%                   "    �� �%    � " ;          "      " ;      4               "    ߱� �&     " &     � *                " &     � �&     z     " &                " & 
    � �&     z     " &     " &     &    &    * &              " &     � �&     z     " 6     " &     " &   � " &   � " & 	  � &    &    &    &    &    &    0        %              %              %              * 7              " &     � �&     z     " 7                " &    � �&     z     " 7                " & 	   � �&     z     " 7 	               " 7   	 � �&     z     " 7         "    �� �%    �      " 
     � *                "      " 
     � *              "      " 
     � *              "      " 
     � *              "      " 
     � *              "      " 
     � *              "      " 
     � *              "      " 
     � *       " 
 -  %                  " 
 0  %                  "    � �%    � " ;          "      " ;      4               "    ߱� �&     " 1     � *      H     4               "      " ;     � �&     " %     � *         " 
 1  %                  "    � �%    �      " ;     � *                "      " ;     � *          "      " ;     "     � " ;   � &    &    &    &        %              %              * <    4               "    ߱� �&     " <     � *          "      � *                "      � �%      � *   �"    �&    &     *    "       $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %              " 
   � " 
   � " 
   � &    &    0        %              %              8    " =     &    " =     &    &    " =   � &    &    ( <       " 
   �%                    '    " 
   �" 
   �%               �            B �     �     �     �     t     `     L       $        � ,     � �*          " 5     � �*     � D&     " 5     � D&     " 5     � �*  
   " =     � �*          " 
     %              %               %              %               %              %               %              %               %              %               %              %               %              %               %              � �%      " =               "      " 
   �    "    �" 
 *  �  $    4     	    %              " =     %               $    4     
   %              " =     %               $    4        %              " =     %               $    4        %              " = 
    %               $    4       
 %              " =     %               $    4        %              " =     %              T      @   "          %              %               " 
         "    � " 
 *     $    4     	    %              " =     %               $    4     
   %              " =     %               $    4        %              " =     %               $    4        %              " = 
    %               $    4       
 %              " =     %               $    4        %              " =     %              h h    T   "         ,  " 
      " 
          " 
     %               T   "    �   ,  " 
 *     " 
 *  [     " 
 *  �%               $    4     	    %              " =     %               $    4     
   %              " =     %               $    4        %              " =     %               $    4        %              " = 
    %               $    4       
 %              " =     %               $    4        %              " =     %              p h    \   "         , %              %                   " 
   �%               T   "         ,  " 
 *  � " 
 *  �     " 
 *  � %               $    4     	    %              " =     %               $    4     
   %              " =     %               $    4        %              " =     %               $    4        %              " = 
    %               $    4       
 %              " =     %               $    4        %              " =     %              " 
 ,        "    [� �%    � " =          "      " =      4               "    ߱� �&     " 0     � *      H     4               "      " 0     � �&     " 0     � *     "    �&    &     *    "       $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %              � J,   8    "      &    "    "    "    "    "    "    "  	  "  
  "    "    "    "    "    "    "    "    "    "    "    �   \   8      �   �   �   �   `   <      �    �    �    �    d    @            " 	     &        " 	     &        " 	     &        " 	     &        " 	     &        " 	     &        " 	 	    &        " 	 
    &        " 	     & 	       " 	     & 
       " 	     &        " 	     &        " 	     &        " 	     &        " 	     &        " 	     &        " 	     &        " 	     &        " 	     &     * 	   � N,     � j,          " 	     "           " 	     "           " 	     "           " 	     "           " 	     "           " 	     "           " 	     "           " 	     "           " 	     "           " 	     "           " 	 !    "  !         " 	 "    "  "         " 	 #    "  #         " 	 #    "  #         " 	 #    "  #         " 	 &    "  &         " 	 '    "  '         " 	 '    "  '         " 	 '    "  '         " 	 *    "  *         " 	 +    "  +         " 	 ,    "  ,         " 	 -    "  -         " 	 .    "  .         " 	 /    "  /         " 	 0    "  0         " 	 1    "  1         " 	 2    "  2    � �%    � %               " 
     " 
     " 
         " 
     &    " 
    &    &    T <   0        %              %              8    " ?     &     ,   &        S    " ?     &    &    " ?     &    &    � �%    � � �%    � " ?   � " 
   �     " 
     &    " 
        " 
 	    &    " 
 	   &    &    � ,   8 D       %              8    " '     &     4   &    $    4    '     %              &        &    8    " '     &    " '     &    &    " '     " '     &    &    &    &        %              %              " ?   � &    &    ( <       " 
   q%                    '    " 
   q" 
   �%               �            B     �     �     �     �     � $    t     `     L       $        � �,     � �*          " 5     � �*     � D&     " 5     � D&          " 5     � &     � �*  
   " ?     � �,  
   " ?     � �*          " 
     %              %               %              %               %              %               %              %               %              %               %              %               %              %               %              � �%      " ?               "      " 
   �    "    �" 
 *  �  $    4         %              " ?     %               $    4     	   %              " ?     %               $    4     
   %              " ?     %               $    4        %              " ? 	    %               $    4       	 %              " ?     %               $    4        %              " ?     %               $    4        %              " ?     %              T      @   "          %              %               " 
         "    � " 
 *     $    4         %              " ?     %               $    4     	   %              " ?     %               $    4     
   %              " ?     %               $    4        %              " ? 	    %               $    4       	 %              " ?     %               $    4        %              " ?     %               $    4        %              " ?     %              h h    T   "         ,  " 
      " 
          " 
     %               T   "    �   ,  " 
 *     " 
 *       " 
 *  �%               $    4         %              " ?     %               $    4     	   %              " ?     %               $    4     
   %              " ?     %               $    4        %              " ? 	    %               $    4       	 %              " ?     %               $    4        %              " ?     %               $    4        %              " ?     %              p h    \   "         , %              %                   " 
   �%               T   "         ,  " 
 *  � " 
 *  �     " 
 *  � %               $    4         %              " ?     %               $    4     	   %              " ?     %               $    4     
   %              " ?     %               $    4        %              " ? 	    %               $    4       	 %              " ?     %               $    4        %              " ?     %               $    4        %              " ?     %              " 
 ,        "    � �%    �  4               " ?   ߱� �&     " 0     � *      H     4               "      " ?     � �&     " 0     � *      H     4               "      " 0     � �&     " 0     � *         " 
 -  �%              ( (       " 
 0    %                   " 
 1  �%                   "    �� �%    �  4               " ?   ߱� �&     " '     � *      H     4               "      " ?     � �&     " '     � *      H     4               "      " '     � �&     " 1     � *      H     4               "      " '     � �&     " %     � *                "      " '     � *                "      " '     � *         " 
 0  %                  "    � �%    �  4               " '   ߱� �&     " 1     � *      H     4               "      " '     � �&     " 1     � *      H     4               "      " '     � �&     " %     � *         " 
 1  q%                  "    q� �%    �      " '     � *                "      " '     � *     " ' 	    &    &    * 8    H     4               "      " ' 	    � �&     " 8     � *                "      " ' 	    � *     4  '     %              &    &    * (    `     L     8      $   "      4    '     %              � �&     " (     � *      8      $   "      4  ߱'     %              � *     "    �&    &     *    "       $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %              " 
   � " 
   � " 
   � " 
   � " 
 	  � " 
   � &    &    �    x    T    0        %              %              8    " @     &    8    " @     &    8    " @     &    8    " @     &    " @     &    &    " @     &    &    " @     " @     &    &    &    &        %              %              " @   � &    &    ( <       " 
   �%                    '    " 
   �" 
   �%               �            B     �     �     �     �     � $    t     `     L       $        � �,     � �*          " 5     � �*     � D&     " 5     � D&          " 5     � &     � �*  
   " @     � �,     " @     � �*          " 
     %              %               %              %               %              %               %              %               %              %               %              %               %              %               %              � �%      " @               "      " 
   �    "    �" 
 *  �  $    4     	    %              " @ 	    %               $    4     
  	 %              " @     %               $    4        %              " @     %               $    4        %              " @     %               $    4        %              " @     %               $    4        %              " @     %              T      @   "          %              %               " 
         "    � " 
 *     $    4     	    %              " @ 	    %               $    4     
  	 %              " @     %               $    4        %              " @     %               $    4        %              " @     %               $    4        %              " @     %               $    4        %              " @     %              h h    T   "         ,  " 
      " 
          " 
     %               T   "    {   ,  " 
 *     " 
 *  �     " 
 *  �%               $    4     	    %              " @ 	    %               $    4     
  	 %              " @     %               $    4        %              " @     %               $    4        %              " @     %               $    4        %              " @     %               $    4        %              " @     %              p h    \   "         , %              %                   " 
   �%               T   "         ,  " 
 *  � " 
 *  �     " 
 *  � %               $    4     	    %              " @ 	    %               $    4     
  	 %              " @     %               $    4        %              " @     %               $    4        %              " @     %               $    4        %              " @     %               $    4        %              " @     %              " 
 ,        "    � �%    �  4               " @   ߱� �&     " 0     � *      H     4               "      " @     � �&     " 0     � *      H     4               "      " 0     � �&     " 0     � *         " 
 -  �%                  " 
 0  �%                  "    �� �%    �  4               " @   ߱� �&     " 1     � *      H     4               "      " @     � �&     " 1     � *      H     4               "      " @     � �&     " %     � *         " 
 1  �%                  "    �� �%    �      " @     � *                "      " @     � *     " @     &    &     * 8              "      " @     � *      H     4               "      " @     � �&     " 8     � *     " @   � &    &    * (    H     4               "      " @     � �&     " (     � *                "      " @     � *         " 
 .  �%                  "    �� �%    � " @          "      " @     " @     &    &    * )    4               "    ߱� �&     " )     � *          "      � *     "    �&    &     *    "       $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %              %              � �%           " 
     %                  " 
   �� �%    � "                 " 
     � (     "          " 
   �%       �      � �%    � %               " 
     " 
     " 
     " 
         " 
     &    " 
    &    &    x <   T    0        %              %              8    " A     &    8    " A     &     ,   &        S    " A     &    &    " A     &    &    " A   � &    &    " A     &    &    " A   � &    &    ( <       " 
   %                    '    " 
   " 
   �%               �            B ,            �     �     �     �     � $    t     `     L       $        � #-      � �*          " 5     � �*     � D&     " 5     � D&          " 5     � &     � �*  
   " A     � �*  	   " A     � D-  
   " A     � �*          " 
     %              %               %              %               %              %               %              %               %              %               %              %               %              %               %              � �%      " A               "      " 
   �    "    q" 
 *  �  $    4     	    %              " A     %               $    4     
   %              " A     %               $    4        %              " A 	    %               $    4       	 %              " A     %               $    4        %              " A     %               $    4        %              " A     %              T      @   "          %              %               " 
         "    � " 
 *     $    4     	    %              " A     %               $    4     
   %              " A     %               $    4        %              " A 	    %               $    4       	 %              " A     %               $    4        %              " A     %               $    4        %              " A     %              h h    T   "         ,  " 
      " 
          " 
     %               T   "       ,  " 
 *     " 
 *  Z     " 
 *  �%               $    4     	    %              " A     %               $    4     
   %              " A     %               $    4        %              " A 	    %               $    4       	 %              " A     %               $    4        %              " A     %               $    4        %              " A     %              p h    \   "         , %              %                   " 
   %               T   "         ,  " 
 *  � " 
 *  �     " 
 *  � %               $    4     	    %              " A     %               $    4     
   %              " A     %               $    4        %              " A 	    %               $    4       	 %              " A     %               $    4        %              " A     %               $    4        %              " A     %              " 
 ,        "    Z� �%    �  4               " A   ߱� �&     " 0     � *      H     4               "      " A     � �&     " 0     � *      H     4               "      " 0     � �&     " 0     � *         " 
 +  [%              " &     " & 
    " &     " &     " &     " & 	    " &     � �%          " 
 /  [%                   "    [� �%    � " A          "      " A      4               "    ߱� �&     " &     � *                " &     � �&     z     " &                " & 
    � �&     z     " &     " &     &    &    * 6              " &     � �&     z     " 6     " &     " &   � " &   � " & 	  � &    &    &    &    &    &    0        %              %              %              * 7              " &     � �&     z     " 7                " &    � �&     z     " 7                " & 	   � �&     z     " 7 	               " 7   	 � �&     z     " 7         "    �� �%    �      " 
     � *                "      " 
     � *              "      " 
     � *              "      " 
     � *              "      " 
     � *              "      " 
     � *              "      " 
     � *              "      " 
     � *       " 
 .  [%                  "    [� �%    �  4               " A   ߱� �&     " )     � *      H     4               "      " A     � �&     " )     � *     "    �&    &     *    "       $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %              %               � �%          " 
   �%               � �%    � %               " 
     " 
     " 
     " 
         " 
     &    " 
    &    &    x <   T    0        %              %              8    " A     &    8    " A     &     ,   &        S    " A     &    &    " A     &    &    " A   � &    &    " A     &    &    " A   � &    &    ( <       " 
   �%                    '    " 
   �" 
   �%               �            B ,            �     �     �     �     � $    t     `     L       $        � #-      � �*          " 5     � �*     � D&     " 5     � D&          " 5     � &     � �*  
   " A     � �*  	   " A     � D-  
   " A     � �*          " 
     %              %               %              %               %              %               %              %               %              %               %              %               %              %               %              � �%      " A               "      " 
   �    "    �" 
 *  �  $    4     	    %              " A     %               $    4     
   %              " A     %               $    4        %              " A 	    %               $    4       	 %              " A     %               $    4        %              " A     %               $    4        %              " A     %              T      @   "          %              %               " 
         "    � " 
 *     $    4     	    %              " A     %               $    4     
   %              " A     %               $    4        %              " A 	    %               $    4       	 %              " A     %               $    4        %              " A     %               $    4        %              " A     %              h h    T   "         ,  " 
      " 
          " 
     %               T   "    �   ,  " 
 *     " 
 *  q     " 
 *  �%               $    4     	    %              " A     %               $    4     
   %              " A     %               $    4        %              " A 	    %               $    4       	 %              " A     %               $    4        %              " A     %               $    4        %              " A     %              p h    \   "         , %              %                   " 
   �%               T   "         ,  " 
 *  � " 
 *  �     " 
 *  � %               $    4     	    %              " A     %               $    4     
   %              " A     %               $    4        %              " A 	    %               $    4       	 %              " A     %               $    4        %              " A     %               $    4        %              " A     %              " 
 ,        "    �� �%    �  4               " A   ߱� �&     " 0     � *      H     4               "      " A     � �&     " 0     � *      H     4               "      " 0     � �&     " 0     � *         " 
 +  �%              " &     " & 
    " &     " &     " &     " & 	    " &     � �%          " 
 /  �%                   "    �� �%    � " A          "      " A      4               "    ߱� �&     " &     � *                " &     � �&     z     " &                " & 
    � �&     z     " &     " &     &    &    * 6              " &     � �&     z     " 6     " &     " &   � " &   � " & 	  � &    &    &    &    &    &    0        %              %              %              * 7              " &     � �&     z     " 7                " &    � �&     z     " 7                " & 	   � �&     z     " 7 	               " 7   	 � �&     z     " 7         "    q� �%    �      " 
     � *                "      " 
     � *   Z           "      " 
     � *   Z           "      " 
     � *   Z           "      " 
     � *   Z           "      " 
     � *   Z           "      " 
     � *   Z           "      " 
     � *   Z    " 
 .  Z%                  "    Z� �%    �  4               " A   ߱� �&     " )     � *      H     4               "      " A     � �&     " )     � *     "    �&    &     *    "       $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %              " 
   � " 
   � " 
   � " 
   � &    &    T    0        %              %              8    " =     &    8    " =     &    " =     &    &    " =     &    &    " =   � &    &    ( <       " 
   [%                    '    " 
   [" 
   �%               �            B     �     �     �     �     � $    t     `     L       $        � c-     � �*          " 5     � �*     � D&     " 5     � D&          " 5     � &     � �*  
   " =     � D-  
   " =     � �*          " 
     %              %               %              %               %              %               %              %               %              %               %              %               %              %               %              � �%      " =               "      " 
   �    "    " 
 *  �  $    4     	    %              " =     %               $    4     
   %              " =     %               $    4        %              " =     %               $    4        %              " = 
    %               $    4       
 %              " =     %               $    4        %              " =     %              T      @   "          %              %               " 
         "    � " 
 *     $    4     	    %              " =     %               $    4     
   %              " =     %               $    4        %              " =     %               $    4        %              " = 
    %               $    4       
 %              " =     %               $    4        %              " =     %              h h    T   "         ,  " 
      " 
          " 
     %               T   "    �   ,  " 
 *     " 
 *  �     " 
 *  �%               $    4     	    %              " =     %               $    4     
   %              " =     %               $    4        %              " =     %               $    4        %              " = 
    %               $    4       
 %              " =     %               $    4        %              " =     %              p h    \   "         , %              %                   " 
   �%               T   "         ,  " 
 *  � " 
 *  �     " 
 *  � %               $    4     	    %              " =     %               $    4     
   %              " =     %               $    4        %              " =     %               $    4        %              " = 
    %               $    4       
 %              " =     %               $    4        %              " =     %              " 
 ,        "    �� �%    �  4               " =   ߱� �&     " 0     � *      H     4               "      " =     � �&     " 0     � *      H     4               "      " 0     � �&     " 0     � *         " 
 .  �%                  "    �� �%    � " =          "      " =      4               "    ߱� �&     " )     � *     "    �&    &     *    "       $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4     
    %              %               $ $   4         %              4     	    %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4         %              4         %              %               $ $   4     	    %              4         %              %                              �           �   l       ��                 }  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       DN     
 !                   � ߱              �  (  �      �N      4   �����N                �                      ��                  �  �                  `��                       �  8  �  �  �  �N            �  �  `      @O      4   ����@O                p                      ��                  �  �                  ���                       �  �  �  o   �  !    ,                                 �  �   �  `O      �  �   �  �O      $  $  �  �  ���                       �O     
 !                   � ߱        8  �   �  �O      L  �   �  �O      `  �   �  P          $   �  �  ���                       HP  @         4P              � ߱                   !  T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����   !     ��                            ����                                            �           �   l       ��                 �    �               <                    O   ����    e�          O   ����    R�          O   ����    ��      �#   "                   �          �  $  �    ���                       �P     
 "                   � ߱                  �  �                      ��                   �  �                  ���                     �  4      4   �����P      $  �  �  ���                       Q     
 "                   � ߱        �    �  4  D      Q      4   ����Q      /  �  p         "                      3   ����0Q  �  �   �  <Q          O     ��  ��  tQ             "                  , �                          
                               � "     ��                            ����                                                        �   l       ��                  	    �               P�\                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ���                 F  �               D�\                    O   ����    e�          O   ����    R�          O   ����    ��      �   �        �   �        |    !  �   l      �r      4   �����r                |                      ��                  !  (                  0�                       !     <     "  �  �  ��                                                3   �����r                �  �      d  L      ��                  #  &  |              ��                �     #  �      O   ����  e�          O   ����  R�          O   ����  ��      �  9   $          �                      � ߱            \   %  �  ���                            P   '     ��        �      ,          �  �      ��                  )  +                �                D     )        �         ��                            7   ����         ��               �r    �            X                  6   )       �   ��         |  �r    �            X                                                        �r                 �  �                                   @            �   �        O   ����  e�          O   ����  R�          O   ����  ��          :   *             �  A  ,        �   ��         	                                                        �  �                                   @            �   �    l    -    �      �r      4   �����r                �                      ��                  -  0                  x0p                       -    �  9   .      s                         � ߱            $  /  �  ���                             |      	  �      �  �      ��                  3  =  �              1p                �     3         �  �       ��                            7   ����         ��          
           �            H                  6   3       l   ��         
           �            H                                                                �  �                                   @            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �	  A  4       ' l	   ��         `	                                            s                 �	  �	           s            s         �            �	   �	    
    6  �	  �	      (s      4   ����(s      O   6  �� ��      �
  A  7        h
  	 ��         \
                                             4s                 �
  �
      	     @s      	     Hs         �            �
   �
          9  �
  \      Ps      4   ����Ps                l                      ��                  9  <                  l�                       9  �
  �  9   :     \s                         � ߱            $  ;  |  ���                       �  $  ?     ���                       hs      
                   � ߱              �      <            �      ��                  @  C  $              �                       @  ,      �  $       ��                            7   ����        	 ��                     �            t                  6   @       �  	 ��                    �            t                                                                �  �      	             	                @            �   �        O   ����  e�          O   ����  R�          O   ����  ��            A  X  h  �  ts      4   ����ts      $  A  �  ���                       �s      
                   � ߱            $  B  �  ���                       �s      
                   � ߱          ��                             ��                             ��                             ��                             ��                            ����                            �  '      =   =        	     =   &                           �           �   l       ��               L  �  �               �ep                    O   ����    e�          O   ����    R�          O   ����    ��      �   �   U     �    `  �   \      �s      4   �����s                l                      ��                  `  i                  hmp                       `  �   ,     a  �  �  ��                                                3   ���� t                �        T  <      ��                  b  g  l              �mp                (     b  �      O   ����  e�          O   ����  R�          O   ����  ��           �   *                   � ߱        �  \   c  �  ���                        �  A  d       8  
 ��         ,                                             t                 �  t      
     t      
      t         �            T   d    �    e  �  �      (t      4   ����(t      9   e         $  f  �  ���                       4t                         � ߱            P   h     ��        �      X          (        ��                  k  m  @              0M�                p     k  <      �  4       ��                            7   ����        
 ��                     �            �                  6   k       �  
 ��         �        �            �                                                        @t                 �  �      
     Lt      
     Tt         �            �   �        O   ����  e�          O   ����  R�          O   ����  ��          :   l               A  o        �  
 ��                                                                   �      
             
                @            �   �    �    p  8  �      \t      4   ����\t                �                      ��                  p  s                  �M�                       p  H     9   q     ht                         � ߱            $  r  �  ���                             �      <
          
  �	      ��                  u  y  $
              (N�                       u  ,      �  $	       ��                            7   ����        
 ��                     �            t	                  6   u       �	  
 ��                    �            t	                                                                �	  �	      
             
                @            �	   �	        O   ����  e�          O   ����  R�          O   ����  ��      �
  A  v       & �
   ��         �
                                            tt                 �
  �
           �t           �t         �            �
   �
          x           �t      4   �����t      :   x                         *  l                                      *     ��                             ��                             ��                             ��                            ����                            ,  &      =   g          
                 �           �   l       ���7          #     �  �  �                R�                    O   ����    e�          O   ����    R�          O   ����    ��      �  �   �  �t      �t      
               �t      
               �t      
               �t      
               �t      
               �t      
 	       	       u      
               u      
               $u      
                   � ߱           $  �  �   ���    	                   �    �    ,      8u      4   ����8u      $  �  X  ���                       pu      
                   � ߱            �  �  �      �u      4   �����u      $  �  �  ���                       �u      
                   � ߱        �    �  $  4      �u      4   �����u      $  �  `  ���                       �u      
                   � ߱            �  �  �      �u      4   �����u      $  �  �  ���                       v      
                   � ߱        �    �  ,  <      @v      4   ����@v      $  �  h  ���                       xv      
 	       	           � ߱            �  �  �      �v      4   �����v      $  �  �  ���                       �v      
                   � ߱        �    �  4  D      �v      4   �����v      $  �  p  ���                       �v      
                   � ߱        �  /   �  �                                 3   �����v    /   �                                   3   ���� w  ,	    �  0  �  �  $w      4   ����$w                (                      ��                  �  �                  �~�                       �  @    8      �  �                      ��        0         �  �                  Ё�    +  �w            �  �      $  �  d  ���                       tw      +                   � ߱        �  $  �  �  ���                       �w      +                   � ߱            4   �����w        �    $  |  x      4   ����x      $  �  P  ���                       (x      ,                   � ߱            $  �  �  ���                       Hx      ,                   � ߱            $  �   	  ���                       �x      ,                   � ߱        �	  �   �     �x      
               �x      
                   � ߱        
  $  �  <	  ���                         ,
      �
  �
                      ��        0         �  �                  (��    +  hy          �  �	      $  �  X
  ���                       �x      +                   � ߱        �
  $  �  �
  ���                       y      +                   � ߱            4   ����@y  D  $  �    ���                       |y      
                   � ߱        �    �  `  �      �y      4   �����y                �                      ��                  �  �                  ���                       �  p  (  /   �                                   3   ����|z      $  �  T  ���                       �z      +                   � ߱        �    �  �        �z      4   �����z                (                      ��                  �  �                  ��                       �  �  d  /   �  T                                 3   ����<{      $  �  �  ���                       \{      +                   � ߱        �    �  �  T      p{      4   ����p{                d                      ��                  �  �                  ,��                       �  �        �  �  �      �{      4   �����{                                      ��                  �  �                  ���                       �  �  H  /   �  8                                 3   ����P|      $  �  t  ���                       p|      +                   � ߱        D    �  �  8      �|      4   �����|                H                      ��                  �  �                  ���                       �  �  �    �  d  t  �  }      4   ����}      /   �  �                                 3   ����d}      /   �  �                                 3   �����}      $  �    ���                       �}      +                   � ߱        (    �  `  �      �}      4   �����}  	              �                      ��             
     �  �                  ���                       �  p        �    �      D~      4   ����D~  
              �                      ��             
     �  �                  <��                       �    �  /   �  �                                 3   �����~      $  �  �  ���                       �~      +                   � ߱        d    �  D  �      �~      4   �����~                �                      ��                  �  �                  অ                       �  T    /   �  �                                 3   ����X      $  �  8  ���                       |      +                   � ߱        �    �  �  �      �      4   �����                                      ��                  �  �                  |��                       �  �  H  /   �  8                                 3   �����      $  �  t  ���                       8�      +                   � ߱              �  �  �      L�      4   ����L�      /   �  �                                 3   ����t�  �  /   �  4     D                          3   ������  t        d                      3   ������  �        �                      3   ������  ,        �  �                  3   �����      $   �     ���                                
                   � ߱                  L  \                  3   �����      $   �  �  ���                                
                   � ߱         �      
                   � ߱        x  $  �  �  ���                         �      �  H                      ��        0         �  #                  d��    +  ܁     d$     �        $  �  �  ���                       \�      +                   � ߱        8  $  �    ���                       ��      +                   � ߱            4   ������  �  $  �  t  ���                       ��      
                   � ߱        �    �  �  8      8�      4   ����8�                H                      ��                  �  �                  �                       �  �  �  /   �  t                                 3   ������      $  �  �  ���                       �      +                   � ߱            �  �  t      (�      4   ����(�                �                      ��                  �  �                  ���                       �    �  /   �  �                                 3   ������      $  �  �  ���                       Ѓ      +                   � ߱        �       4  �      �      4   �����                �                      ��                                      (��                         D          �  X      l�      4   ����l�                h                      ��                                      ���                         �  �  /     �                                 3   ����Ą      $    �  ���                       �      +                   � ߱        �        �      ��      4   ������                �                      ��                  	                    P��                       	  (  H    
  �  �    ��      4   ������      /     �                                 3   ����؅      /     8                                 3   ������      $    t  ���                       �      +                   � ߱        �!      �  8       0�      4   ����0�                H                       ��                                      ��                         �          d   �       ��      4   ������                �                       ��                                      h��                         t   ,!  /     !                                 3   �����      $    X!  ���                       0�      +                   � ߱        �"      �!  "      D�      4   ����D�                ,"                      ��                                      ��                         �!  h"  /     X"                                 3   ����̇      $    �"  ���                       ��      +                   � ߱        �#      �"  X#      �      4   �����                h#                      ��                                       ���                         �"  �#  /     �#                                 3   ������      $    �#  ���                       ��      +                   � ߱              "  $  ($      ��      4   ������      /   "  T$                                 3   �����  x$  �   %  �      @%  A  (       - �$   ��         �$  \�                                         $�   0�                   ,%   %           <�  L�           D�  T�                      �$   %    �+    +  \%  �%      ��      4   ������                T&                      ��                  +  E                  ��                       +  l%        d&      (          �'  �'      ��                  ,  D  �'              x��                       ,  �%      �&  �&       ��                            7   ����         ��               ��    �            0'                  6   ,       `'   ��         T'  ��    �            0'                                                        ��                 �'  �'                                   @            |'   �'        O   ����  e�          O   ����  R�          O   ����  ��      �(  A  -       - x(   ��         T(  H�                                         ��   Љ   ܉   �   �                 �(  �(           �  (�  8�            �  0�  @�         �            �(   �(          1   )  |)      ��      4   ������                \+                      ��                  1  C                  P��                       1  )  ��                     ��                     Ȋ                     ܊                       ��       #       #       �       $       $       �       %       %       ,�       &       &       @�       )       )       T�       *       *       h�       +       +       |�       ,       ,       ��       /       /       ��       0       0       ��       1       1       ̋       2       2           � ߱            $  2  �)  ���                       ,    F  �+  �+      ��      4   ������      /   F  �+                                 3   �����  (�                         � ߱        H,  $  K  �+  ���                       H-    U  d,  �,      4�      4   ����4�                -                      ��                  U  Y                  ��                       U  t,  \�                         � ߱            $  V  �,  ���                       H/    Z  d-  �-      ��      4   ������                �-                      ��                  Z  c                  ���                       Z  t-  /    [  .  �.      �      4   �����                �.                      ��                  [  _                  ��                       [  .  @�                         � ߱            $  \  �.  ���                       ԍ                         � ߱            $  `  �.  ���                       �3    d  d/  �/      h�      4   ����h�                �/                      ��             !     d  y                  ���                       d  t/  �3    e  0  �0  \1  ��      4   ������                �0                      ��                  e  i                  ,��                       e  0  �                         � ߱            $  f  �0  ���                                     l1                      ��             !     j  u                  L��                       j  �0  l2    k  �1  2      |�      4   ����|�                 @2                      ��                   k  o                  Ȗ�                       k  �1  ��                         � ߱            $  l  2  ���                             p  �2  3      8�      4   ����8�  !              @3                      ��             !     p  t                  T��                       p  �2  `�                         � ߱            $  q  3  ���                       ��                         � ߱            $  v  l3  ���                       �4    z  �3  \4      ��      4   ������  "              �4                      ��             "     z  ~                  ���                       z  �3  ��                         � ߱            $  {  l4  ���                       D�                         � ߱        5  $    �4  ���                       H6    �  85  �5      ��      4   ������  #              �5                      ��             #     �  �                  ���                       �  H5  �                         � ߱            $  �  �5  ���                       ,�                         � ߱            $  �  6  ���                                   ,  �6                                             +  7           7  7   , �6                                                                + ,   ��                             ��                            ����                                -                  �           �   l       ��                  �  �  �               `��                    O   ����    e�          O   ����    R�          O   ����    ��           �  �   �       L�      4   ����L�      n   �     �          ��        �    ,      ��      4   ������      �   �  ��    ��                            ����                                            4          �   l       ��                  �  �  �               �Ro                    O   ����    e�          O   ����    R�          O   ����    ��      ��  �          ̓  �          ؓ  �          �  �          �  �          ��  �          �  �          �  � 	          �  �          ,�  �          8�  �          D�  �          P�  �          \�  �          h�  �          t�  �          ��  �          ��  �          ��  �          ��  �          ��  �          ��  �          Ȕ  �              � ߱        @  Z   �  �    �        ��                  �              �              �              �              �              �               �              �              �              �              �              �              �              � ߱        l  h   �  `   �        Ԕ                  
   �  �� �             ��    ��                              ��        	                  ����                                            �           �   l       ��            
     �  �  �               �So                    O   ����    e�          O   ����    R�          O   ����    ��        $   �  �   ���                        �  @         �              � ߱        �  �   �           �      ,  �)      �  �      ��             
     �  �                TY^                       �        �         ��                            7   ����         ��                     �            d                  6   �       �   ��                    �            d                                                                �  �                                   @            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �  $  �  X  ���                       �      .                   � ߱        �  9   �     L    �  �  ,       �      4   ���� �                <                      ��                  �  �                  �Z^                       �  �  �  $  �  h  ���                       H�      .                   � ߱        �  $  �  �  ���                       t�                         � ߱        D  $  �    ���                       ��      .                   � ߱        �  $  �  p  ���                       ��      .                   � ߱        �  $  �  �  ���                       ԕ                         � ߱            $  �     ���                       ��      .                   � ߱        �    �  h  �      �      4   �����                �                      ��                  �                    |[^                       �  x  �    �    �      0�      4   ����0�                �                      ��                  �  �                  @��                       �     �  $  �  �  ���                       X�      .                   � ߱        L  $  �     ���                       ��                         � ߱            $  �  x  ���                       ��      .                   � ߱        �  $     �  ���                       ��      .                   � ߱        T	  $    (	  ���                       �                         � ߱        �	  $    �	  ���                       �      .                   � ߱        
  $    �	  ���                       �      .                   � ߱        \
  $    0
  ���                       D�                         � ߱        �
  $    �
  ���                       P�      .                   � ߱          $    �
  ���                       x�      .                   � ߱        d  $  	  8  ���                       ��                         � ߱        �  $  
  �  ���                       ��      .                   � ߱          $    �  ���                       ؗ      .                   � ߱        l  $    @  ���                       �                         � ߱        �  $    �  ���                       �      .                   � ߱          $    �  ���                       8�      .                   � ߱        t  $    H  ���                       d�                         � ߱        �  $    �  ���                       p�      .                   � ߱        $  $    �  ���                       ��      .                   � ߱        |  $    P  ���                       Ę                         � ߱        �  $    �  ���                       И      .                   � ߱        ,  $       ���                       ��      .                   � ߱        �  $    X  ���                       $�                         � ߱            $    �  ���                       0�      .                   � ߱        H      �  t      X�      4   ����X�                �                      ��             	       M                  l��                           8      �    �  ��      4   ������                ,                      ��                    2                  �l^                         �  �  $    X  ���                       ؙ      .                   � ߱        �  $     �  ���                       �                         � ߱        4  $  !    ���                       �      .                   � ߱        �  $  #  `  ���                       8�      .                   � ߱        �  $  $  �  ���                       d�                         � ߱        <  $  %    ���                       p�      .                   � ߱        �  $  '  h  ���                       ��      .                   � ߱        �  $  (  �  ���                       Ě       	       	           � ߱        D  $  )    ���                       К      .                   � ߱        �  $  +  p  ���                       ��      .                   � ߱        �  $  ,  �  ���                       $�       
       
           � ߱        L  $  -     ���                       0�      .                   � ߱        �  $  /  x  ���                       X�      .                   � ߱        �  $  0  �  ���                       ��                         � ߱            $  1  (  ���                       ��      .                   � ߱                      �                      ��             	     3  D                  `m^                       3  T  �    4  �  h      ��      4   ������                x                      ��                  4  =                  �m^                       4  �  �  $  6  �  ���                       ��      .                   � ߱        (  $  7  �  ���                       �                         � ߱        �  $  8  T  ���                       �      .                   � ߱        �  $  :  �  ���                       @�      .                   � ߱        0  $  ;    ���                       l�       	       	           � ߱            $  <  \  ���                       x�      .                   � ߱              >  �         ��      4   ������  	              0                      ��             	     >  C                  �n^                       >  �  �  $  @  \  ���                       Ȝ      .                   � ߱        �  $  A  �  ���                       ��       
       
           � ߱            $  B    ���                        �      .                   � ߱        �  $  F  d  ���                       (�      .                   � ߱        �  $  G  �  ���                       T�                         � ߱        @  $  H    ���                       `�      .                   � ߱        �  $  J  l  ���                       ��      .                   � ߱        �  $  K  �  ���                       ��                         � ߱            $  L    ���                       ��      .                   � ߱        �    N  d  �      �      4   �����  
              �                      ��             
     N  S                  lo^                       N  t  H  $  P    ���                       �      .                   � ߱        �  $  Q  t  ���                       <�                         � ߱            $  R  �  ���                       H�      .                   � ߱        P  $  T  $  ���                       p�                         � ߱        �  $  U  |  ���                       ��                         � ߱            $  V  �  ���                       ��                         � ߱        X   $  W  ,   ���                       ܞ                         � ߱        �   $  X  �   ���                        �                         � ߱        !  $  Y  �   ���                       $�                         � ߱        `!  $  Z  4!  ���                       H�       (       (           � ߱        �!  $  [  �!  ���                       l�       '       '           � ߱        "  $  \  �!  ���                       ��       "       "           � ߱        h"  $  ]  <"  ���                       ��       !       !           � ߱        �"  $  ^  �"  ���                       ؟       .       .           � ߱        #  $  _  �"  ���                       ��       -       -           � ߱        p#  $  `  D#  ���                        �                         � ߱        �#  $  a  �#  ���                       D�                         � ߱         $  $  b  �#  ���                       h�       *       *           � ߱        x$  $  c  L$  ���                       ��       )       )           � ߱        �$  $  d  �$  ���                       ��       $       $           � ߱        (%  $  e  �$  ���                       Ԡ       #       #           � ߱        �%  $  f  T%  ���                       ��       0       0           � ߱        �%  $  g  �%  ���                       �       /       /           � ߱        0&  $  h  &  ���                       @�                           � ߱        �&  $  i  \&  ���                       d�                         � ߱        �&  $  j  �&  ���                       ��       ,       ,           � ߱        8'  $  k  '  ���                       ��       +       +           � ߱        �'  $  l  d'  ���                       С       &       &           � ߱        �'  $  m  �'  ���                       ��       %       %           � ߱        @(  $  n  (  ���                       �       2       2           � ߱            $  o  l(  ���                       <�       1       1           � ߱                   .  )          �(  )   @ �(                                                             0              0        .     ��                              ��        	                   ��                            ����                                =   �                     �           �   l       ��                  �  �  �               �^                    O   ����    e�          O   ����    R�          O   ����    ��      �     �  `�  }          O   �  ��  ��  t�    ��                            ����                                                      �   l       ���               �  �  �               ��^                    O   ����    e�          O   ����    R�          O   ����    ��      ��      
               ��      
 *       *       ��                         � ߱        D  $  �  �   ���                         A  �       / �   ��         �   �                                         Ȣ   Ԣ                   �  �           �  �           �  ��                      �   �    t	    �  (  �  �  0�      4   ����0�                                       ��                  �  �                  ���                       �  8        0      8            �      ��                  �  �                 ��                       �  �  �  \  �       ��                            7   ����    /      ��               p�    �            �                  6   �       / 4   ��            p�    �            �                                                        8�   D�                   �  x           P�  `�           X�  h�                      P   d        �         ��$                           A   ����    0      ��                     �            d                  6   �       0 �   ��         �        �            d                          *                              ��                 �  �           ��           ��         �            �   �        O   ����  e�          O   ����  R�          O   ����  ��          $  �  d  ���                       ��                         � ߱                      x                      ��                  �  �                  P��                       �  �        �      	          �  �      ��                  �  �  	              @��                       �        �         ��                            7   ����    0      ��                     �            T                  6   �       0 x   ��                    �            T                                                                �  �                                   @            �   �        O   ����  e�          O   ����  R�          O   ����  ��          $  �  H	  ���                       �                         � ߱        �	  �   �  t�      L
  $  �  �	  ���                       ��      
                   � ߱              \
                �  �  �  ��           ��  �  �  �              ���                H     �  �	      �
  �
       ��    �                      7   ����    1      2                      �            (                  6   �       1 L   2                     �            (                                                                �  �                                   @            h   x        1 2     8   �  2       O   ����  e�          O   ����  R�          O   ����  ��            �  $  4      �      4   �����      �   �  4�          /   �  t                                3   ����|�              3  �                                   �� 3     ��                             ��                             ��                              ��        	                   ��                            ����                                /                  �           �   l       ���j          !     �  '  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      ��      
               ��      
                   � ߱        �  $  �  �   ���                             �      8            �      ��                  �  
                  ��                \7     �  (      �          ��                            7   ����         
 ��                     �            p                  6   �        �  
 ��                    �            p                                                                �  �      
             
                @            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �  $  �  d  ���                       ��      
                   � ߱        l    �  �  �    إ      4   ����إ      $  �  �  ���                       ��      
                   � ߱            $  �  @  ���                       �      
                   � ߱              �  �        8�      4   ����8�                �                      ��                  �  	                  ň                       �  �        �      �  �j      �  �      ��                    �  �              �ň                07         D  �         ��                            7   ����    4      ��               ��    �            \                  6          4 �   ��         �  ��    �            \                                                        `�   l�   ��   ��   ��   ��   ��   Ħ  	 Ц  
 �   ��   �    �   <�   H�   d�                   0  $           p�           x�                             �  p  �       ��$                           A   ����    5      ��                     �                              6          5 @   ��         4        �                                      *                              ��                 �  |           ��           ��         �            \   l    �	  �  	       ��$                           A   ����    0      ��                     �            h	                  6          0 �	   ��         �	        �            h	                          *                              ��                 �	  �	           Ȩ           Ш         �            �	   �	    L   
  p
       ��$                           A   ����    &      ��                      �            �
                  6          & �
   ��          �
        �            �
                          *                              ب                 8  ,           �           �         �                   �  x  �       ��$                           A   ����    '      ��          !     ��    �                              6          ' �   ��  
      ! <  ��    �                                      *                              ��    �   �   �   $�   @�   L�   h�  	 t�  
 ��                   �  �           ��           ��         �            �   �    4    X       ��$                           A   ����    1      ��          "           �            �                  6          1 �   ��        " �        �            �                          *                              t�                               ��           ��         �            �           `  �       ��$                           A   ����    %      ��          #     Ȫ    �                               6          % 8   ��        # $  Ȫ    �                                       *                              ��   ��                   �  |           ��  ��           ��  ��         �            T   h        O   ����  e�          O   ����  R�          O   ����  ��      h               ��      4   ������      $     <  ���                       x�  @         d�              � ߱        �  $    �  ���                       ��      
                   � ߱        ��       	 ��̬  	       �       
 ����  
       �        ���         0�        ��D�         X�        ��l�         ��        ����         ��        ����             � ߱        �  $    �  ���                         $  #  �  ���                       Э                         � ߱        p  $  $  D  ���                       ܭ                         � ߱            &  �        �      4   �����                �                      ��                  &  /                  �v�                       &  �  0�          h�         |�       	   ��  	       Ȯ       
    �  
       �          L�         `�          ��         ��          �         ��          0�             � ߱            $  '    ���                       �    1  4  �      D�      4   ����D�                �                      ��                  1  :                  H~j                       1  D  ��          ��         �       	   D�  	       X�       
   ��  
       ��          ܱ         �          (�         <�          t�         ��          ��             � ߱            $  2  �  ���                       h    <  �  X      Բ      4   ����Բ                <                      ��                  <  E                  t�j                       <  �  ��          �         ��       	   0�  	       D�       
   |�  
       ��          ȴ         ܴ          �         (�          `�         t�          ��             � ߱            $  =  h  ���                           G  �         ��      4   ������                �                      ��                  G  P                  ԓj                       G  �  ��          ض         �       	   $�  	       8�       
   p�  
       ��          ��         з          �         �          T�         h�          ��             � ߱            $  H    ���                       �    [  ,  �      ��      4   ������                �                      ��                  [  _                  ��j                       [  <  �    \  �  �  <  ��      4   ������      $  \    ���                       �                         � ߱            $  ]  h  ���                       �                         � ߱            $  ^  �  ���                       �                         � ߱        h%    `    �      T�      4   ����T�  	              �                      ��             
     `  �                  4�j                       `    |�      
               ��      
               ��      
               ��      
               ��      
               ��      
               Ĺ      
               й      
                   � ߱        �  $  a  �  ���                       �    j  �  H      ܹ      4   ����ܹ  
              X                      ��             
     j  n                  ��j                       j  �  4    k  t  �  �  �      4   �����      $  k  �  ���                       $�                         � ߱            $  l    ���                       0�                         � ߱            $  m  `  ���                       P�                         � ߱        �  $  p  �  ���                       ��      
                   � ߱        <  $  r    ���                       Ժ      
                   � ߱        �  A  t       6 �   ��        $ �                                            �                 �  �           �           $�         �            �   �    �     u         x   ,�      4   ����,�      $  u  L   ���                       4�      
                   � ߱            $  v  �   ���                       p�      
                   � ߱        �!  A  x       7 8!   ��        %  !  л                                        |�   ��   ��                 �!  �!           ��  ��  ��           ��  ��  Ȼ         �            T!   l!    |"    |  �!  P"      �      4   �����  $�      
               `�      
               ��      
               ؼ      
                   � ߱            $  }  �!  ���                       X#    �  �"  �"   #  �      4   �����      $  �  �"  ���                       4�                         � ߱            $  �  ,#  ���                       T�                         � ߱        �#  $  �  �#  ���                       ��                         � ߱        $  $  �  �#  ���                       ��                         � ߱        `$  $  �  4$  ���                       �                         � ߱        �$  $  �  �$  ���                       $�                         � ߱        %  $  �  �$  ���                       X�                         � ߱            $  �  <%  ���                       ��                         � ߱        P0    �  �%   &      ��      4   ������                &                      ��                  �  �                  8�j                       �  �%  (-    �  ,&  �&  �)  �      4   �����                �&                      ��                  �  �                  ��j                       �  <&  �'    �  �&  �&  <'  @�      4   ����@�      $  �  '  ���                       `�                         � ߱            $  �  h'  ���                       t�                         � ߱        �'  $  �  �'  ���                       ��                         � ߱        D(  $  �  (  ���                       �                         � ߱        �(  $  �  p(  ���                       @�                         � ߱        �(  $  �  �(  ���                       ��                         � ߱            $  �   )  ���                       ��                         � ߱                      �)                      ��                  �  �                  ��                       �  L)  �+    �  �)  `*      �      4   �����                p*                      ��                  �  �                  `��                       �  �)  L+    �  �*  �*  �*  ,�      4   ����,�      $  �  �*  ���                       L�                         � ߱            $  �   +  ���                       ��                         � ߱            $  �  x+  ���                       ��                         � ߱              �  �+  <,      L�      4   ����L�                L,                      ��                  �  �                  ��                       �  �+        �  h,  x,  �,  t�      4   ����t�      $  �  �,  ���                       ��                         � ߱            $  �  �,  ���                       ��                         � ߱        �-  A  �       8 �-   ��        & x-                                            ��                 �-  �-           ��           ��         �            �-   �-    �.    �  �-  .  d.  �      4   �����      $  �  8.  ���                       �                         � ߱            $  �  �.  ���                       h�                         � ߱        t/  A  �       ( /   ��        ' /                                            ��                 `/  T/           ��           ��         �            4/   D/          �  �/  �/  �/  ��      4   ������      $  �  �/  ���                       ��                         � ߱            $  �  $0  ���                       L�                         � ߱        �1    �  l0  �0      ��      4   ������                �0                      ��                  �  �                  ���                       �  |0        �  1  $1  |1  ��      4   ������      $  �  P1  ���                       ��                         � ߱            $  �  �1  ���                       (�                         � ߱        �2  A  �       02   ��        ( $2                                             ��                 x2  l2           ��           ��         �            L2   \2    �6    �  �2  $3      ��      4   ������                43                      ��                  �  �                  ��                       �  �2  D3  9   �         $  �  p3  ���                       ��                         � ߱        ��          �         �          l�         ��          ��         ��          4�         H�          ��         ��          ��         �       	   `�  	       t�          ��         ��          (�         <�          ��         ��          ��         �          T�         h�          ��         ��       	   �  	       0�          ��         ��          ��         ��          H�         \�          ��         ��          �         $�          t�         ��       	   ��  	       ��          <�         P�          ��         ��          �         �          h�         |�          ��         ��          0�         D�       	   ��  	           � ߱            $  �  �3  ���                       ��      
               ��      
                   � ߱            $    �6  ���                               x7  �7      ��      4   ������                p8                      ��             !       %                  ���                         �7        �8      �B  �j      �B  �B      ��             !     $  �  �B              ���                       $  8  4:  �8  �8       ��                            7   ����    4      ��          )     �    �            L9                  6   $       4 �9   ��        ) p9  �    �            L9                                                        ��   ��   �   �   0�   <�   H�   T�  	 `�  
 |�   ��   ��   ��   ��   ��   ��                    :  :            �           �                      �9   :    �;  `:  �:       ��$                           A   ����    5      ��          *           �             ;                  6   $       5 0;   ��        * $;        �             ;                          *                              0�                 x;  l;           <�           D�         �            L;   \;    �<  �;  <       ��$                           A   ����    0      ��          +           �            X<                  6   $       0 �<   ��        + |<        �            X<                          *                              L�                 �<  �<           X�           `�         �            �<   �<    <>  =  `=       ��$                           A   ����    &      ��          ,           �            �=                  6   $       & �=   ��        , �=        �            �=                          *                              h�                 (>  >           t�           |�         �            �=   >    �?  h>  �>       ��$                           A   ����    '      ��          -     <�    �            ?                  6   $       ' p?   ��  
      - ,?  <�    �            ?                          *                              ��   ��   ��   ��   ��   ��   ��   ��  	 �  
  �                   �?  �?           ,�           4�         �            �?   �?    $A  �?  H@       ��$                           A   ����    1      ��          .           �            �@                  6   $       1 �@   ��        . �@        �            �@                          *                              �                 A  A           �           �         �            �@   �@        PA  �A       ��$                           A   ����    %      ��          /     X�    �            �A                  6   $       % (B   ��        / B  X�    �            �A                          *                               �   ,�                   xB  lB           8�  H�           @�  P�         �            DB   XB        O   ����  e�          O   ����  R�          O   ����  ��      XC    3  �B   C      ��      4   ������      $   4  ,C  ���                       �  @         ��              � ߱        �D  $  8  �C  ���                        �      
                   � ߱        H�       	 ��\�  	       p�       
 ����  
       ��        ����         ��        ����         ��        ����         �        ��$�         8�        ��L�             � ߱        �D  $  :  �C  ���                       E  $  B  �D  ���                       `�                         � ߱        `E  $  C  4E  ���                       l�                         � ߱        G    E  |E  �E      x�      4   ����x�                �F                      ��                  E  N                  ��                       E  �E  ��          ��         �       	   D�  	       X�       
   ��  
       ��          ��         ��          (�         <�          t�         ��          ��             � ߱            $  F  F  ���                       �H    P  $G  �G      ��      4   ������                �H                      ��                  P  Y                  (��                       P  4G  P�          ��         ��       	   ��  	       ��       
    �  
       4�          l�         ��          ��         ��          �         �          P�             � ߱            $  Q  �G  ���                       XJ    [  �H  HI      d�      4   ����d�                ,J                      ��                  [  d                  ���                       [  �H  <�          t�         ��       	   ��  	       ��       
   �  
        �          X�         l�          ��         ��          ��         �          <�             � ߱            $  \  XI  ���                        L    f  tJ  �J      P�      4   ����P�                �K                      ��                  f  o                  d��                       f  �J  0�          h�         |�       	   ��  	       ��       
    �  
       �          L�         `�          ��         ��          ��         ��          0�             � ߱            $  g   K  ���                       �M    z  L  �L      D�      4   ����D�                �L                      ��                  z  ~                  |^o                       z  ,L  �M    {  �L  �L  ,M  P�      4   ����P�      $  {   M  ���                       p�                         � ߱            $  |  XM  ���                       |�                         � ߱            $  }  �M  ���                       ��                         � ߱        XX      �M  tN      ��      4   ������                tO                      ��                    �                  (_o                         N  �      
               �      
               $�      
               0�      
               <�      
               H�      
               T�      
               `�      
                   � ߱        �O  $  �  �N  ���                       |Q    �  �O  8P      l�      4   ����l�                HP                      ��                  �  �                  �_o                       �  �O  $Q    �  dP  tP  �P  ��      4   ������      $  �  �P  ���                       ��                         � ߱            $  �  �P  ���                       ��                         � ߱            $  �  PQ  ���                       ��                         � ߱        �Q  $  �  �Q  ���                       (�      
                   � ߱        ,R  $  �   R  ���                       d�      
                   � ߱        �R  A  �       6 �R   ��        0 |R                                            ��                 �R  �R           ��           ��         �            �R   �R    �S    �   S  S  hS  ��      4   ������      $  �  <S  ���                       ��      
                   � ߱            $  �  �S  ���                        �      
                   � ߱        �T  A  �       7 (T   ��        1 T  `�                                        �   �   $�                 �T  tT           0�  @�  P�           8�  H�  X�         �            DT   \T    lU    �  �T  @U      ��      4   ������  ��      
               ��      
               ,�      
               h�      
                   � ߱            $  �  �T  ���                       HV    �  �U  �U  �U  ��      4   ������      $  �  �U  ���                       ��                         � ߱            $  �  V  ���                       ��                         � ߱        �V  $  �  tV  ���                       �                         � ߱        �V  $  �  �V  ���                       L�                         � ߱        PW  $  �  $W  ���                       ��                         � ߱        �W  $  �  |W  ���                       ��                         � ߱         X  $  �  �W  ���                       ��                         � ߱            $  �  ,X  ���                       �                         � ߱        @c    �  tX  �X      P�      4   ����P�                 Y                      ��                  �  �                  ro                       �  �X  `    �  Y  �Y  �\  x�      4   ����x�                �Y                      ��                  �  �                  �ro                       �  ,Y  �Z    �  �Y  �Y  ,Z  ��      4   ������      $  �   Z  ���                       ��                         � ߱            $  �  XZ  ���                       �                         � ߱        �Z  $  �  �Z  ���                       ,�                         � ߱        4[  $  �  [  ���                       t�                         � ߱        �[  $  �  `[  ���                       ��                         � ߱        �[  $  �  �[  ���                       ,�                         � ߱            $  �  \  ���                       `�                         � ߱                      �\                      ��                  �  �                  Pso                       �  <\  �^    �  �\  P]      ��      4   ������                `]                      ��                  �  �                  �so                       �  �\  <^    �  |]  �]  �]  ��      4   ������      $  �  �]  ���                       ��                         � ߱            $  �  ^  ���                       $�                         � ߱            $  �  h^  ���                       ��                         � ߱              �  �^  ,_      ��      4   ������                <_                      ��                  �  �                  �to                       �  �^        �  X_  h_  �_  �      4   �����      $  �  �_  ���                       $�                         � ߱            $  �  �_  ���                       D�                         � ߱        �`  A  �       8 t`   ��        2 h`                                            x�                 �`  �`           ��           ��         �            �`   �`    �a    �  �`  �`  Ta  ��      4   ������      $  �  (a  ���                       ��                         � ߱            $  �  �a  ���                       ��                         � ߱        db  A  �       ( b   ��        3 �a                                            ,�                 Pb  Db           P�           X�         �            $b   4b          �  �b  �b  �b  `�      4   ����`�      $  �  �b  ���                       h�                         � ߱            $  �  c  ���                       ��                         � ߱        �d    �  \c  �c      (�      4   ����(�                 �c                      ��                   �  �                  Xvo                       �  lc        �  d  d  ld  P�      4   ����P�      $  �  @d  ���                       p�                         � ߱            $  �  �d  ���                       ��                         � ߱        |e  A  �        e   ��        4 e                                             �                 he  \e            �           (�         �            <e   Le    �i    �  �e  f      0�      4   ����0�  !              $f                      ��             !     �  �                  wo                       �  �e  4f  9   �         $  �  `f  ���                       <�                         � ߱        H�          ��         ��          ��         �          `�         t�          ��         ��          (�         <�          ��         ��       	   ��  	       �          T�         h�          ��         ��          �         0�          ��         ��          ��         ��          H�         \�       	   ��  	       ��          �         $�          t�         ��          ��         ��          <�         P�          ��         ��          �         �       	   h�  	       |�          ��         ��          0�         D�          ��         ��          ��         �          \�         p�          ��         ��       	   $�  	           � ߱            $  �  �f  ���                         ��                             ��                             ��                              ��        	                   ��                            ����                                =   �         =   �     �j    �j  (  �j  8  �j  7      6                  �           �   l       ���H               -  ,  �               �ƈ                    O   ����    e�          O   ����    R�          O   ����    ��      8�      
               L�      
                   � ߱        �  $  4  �   ���                             �      8            �      ��                  7  4                 ,Ɉ                \&     7  (      �          ��                            7   ����         
 ��          5           �            p                  6   7        �  
 ��         5           �            p                                                                �  �      
             
                @            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �  $  8  d  ���                       X�      
                   � ߱        l    9  �  �    ��      4   ������      $  9  �  ���                       ��      
                   � ߱            $  :  @  ���                       ��      
                   � ߱              ;  �        ��      4   ������                �                      ��                  ;  3                  �o                       ;  �        �      h  �H      8         ��                  T  �  P              ��o                0&     T      �         ��                            7   ����    :      ��          6     ��    �            \                  6   T       : �   ��  	      6 �  ��    �            \                                                        �   �   (�   4�   @�   L�   h�   t�  	 ��                   �           ��           ��             
        
 �   �    p  D  �       ��$                           A   ����    5      ��          7           �            �                  6   T       5    ��        7         �            �                          *                              T�                 \  P           `�           h�         �            0   @    �	  �  �       ��$                           A   ����    0      ��          8           �            <	                  6   T       0 l	   ��        8 `	        �            <	                          *                              p�                 �	  �	           |�           ��         �            �	   �	        �	  D
       ��$                           A   ����    &      ��          9           �            �
                  6   T       & �
   ��        9 �
        �            �
                          *                              ��                               ��           ��         �            �
   �
        O   ����  e�          O   ����  R�          O   ����  ��      �    \  �  �      ��      4   ������      $   ]  �  ���                       (�  @         �              � ߱          $  a    ���                       @�      
                   � ߱        h�       	 ��|�  	       ��       
 ����  
       ��        ����         ��        ����                 ��         0        ��D         X        ��l             � ߱        D  $  c  D  ���                       �  $  k  p  ���                       �                         � ߱        �  $  l  �  ���                       �                         � ߱        �    n    �      �      4   �����                T                      ��                  n  w                  $�o                       n     �       	    	       ,      
   d 
       x         �        �         �                 H        \         �            � ߱            $  o  �  ���                           y  �        �     4   �����               �                      ��                  y  �                  ��o                       y  �  $      	   \ 	       p      
   � 
       �         �                 @        T         �        �         �            � ߱            $  z  (  ���                       �    �  (  �      �     4   �����               l                      ��                  �  �                   �o                       �  8  �      	   � 	             
   H 
       \         �        �         �        �         ,        @         x            � ߱            $  �  �  ���                       $    �  �  0      �     4   �����               �                      ��                  �  �                  ��o                       �  �  l      	   � 	       �      
   � 
       	         <	        P	         �	        �	         �	        �	          
            � ߱            $  �  @  ���                       X    �  @  �      4
     4   ����4
               �                      ��                  �  �                  L�o                       �  P  �    �  �  �  P  @
     4   ����@
     $  �  $  ���                       `
                        � ߱            $  �  |  ���                       l
                        � ߱           $  �  �  ���                       �
                        � ߱            $  �  ,  ���                       �
                        � ߱        �     �  t  �      0     4   ����0 	              �                      ��             
     �  �                   �o                       �  �  X     
               d     
               p     
               |     
               �     
               �     
               �     
               �     
                   � ߱          $  �     ���                       �    �  8  �      �     4   ����� 
              �                      ��             
     �  �                  ��o                       �  H  �    �  �  �  H  �     4   �����     $  �    ���                                                � ߱            $  �  t  ���                                               � ߱            $  �  �  ���                       ,                        � ߱        P  $  �  $  ���                       t     
                   � ߱        �  $  �  |  ���                       �     
                   � ߱        `  A  �       6    ��        : �                                            �                L  @           �                   �                0    <    �  |  �  �       4   ����     $  �  �  ���                            
                   � ߱            $  �    ���                       L     
                   � ߱          A  �       7 �   ��        ; �  �                                       X  d  p                �  �           | � �          � � �        �            �   �    �    �  ,  �      �     4   �����       
               <     
               x     
               �     
                   � ߱            $  �  <  ���                       �    �      l  �     4   �����     $  �  @  ���                                               � ߱            $  �  �  ���                       0                        � ߱          $  �  �  ���                       d                        � ߱        t  $  �  H  ���                       �                        � ߱        �  $  �  �  ���                       �                        � ߱        $   $  �  �  ���                                                � ߱        |   $  �  P   ���                       4                        � ߱            $  �  �   ���                       h                        � ߱        �!  A  �       0!   ��        < $!                                             �                x!  l!           �          �        �            L!   \!    �%    �  �!  $"      �     4   �����               4"                      ��                  �  �                  �p                       �  �!  D"  9   �         $  �  p"  ���                       �                        � ߱        �                  4         �        �         �        �         L        `         �        �                 (      	   x 	       �         �        �         @        T         �        �                          l        �         �        �      	   4 	       H         �        �         �                 `        t         �        �         (        <         �        �      	   � 	                T        h         �        �                 0         �        �         �        �         H        \      	   � 	           � ߱            $  �  �"  ���                       �     
               �     
                   � ߱            $  0  �%  ���                             5  x&  �&      �     4   �����               p'                      ��                  5  *                  ��p                       5  �&        �'      X-  �H      (-  -      ��                  N  �  @-              \�p                       N  '  )  �'  �'       ��                            7   ����    :      ��          =     �   �            L(                  6   N       : �(   ��  	      = p(  �   �            L(                                                            (  4  @  L  h  t 	 �                �(  �(           �          �            
        
 �(   �(    `*  4)  �)       ��$                           A   ����    5      ��          >           �            �)                  6   N       5 *   ��        > �)        �            �)                          *                              T                L*  @*           `          h        �             *   0*    �+  �*  �*       ��$                           A   ����    0      ��          ?           �            ,+                  6   N       0 \+   ��        ? P+        �            ,+                          *                              p                �+  �+           |          �        �            x+   �+        �+  4,       ��$                           A   ����    &      ��          @           �            �,                  6   N       & �,   ��        @ �,        �            �,                          *                              �                �,  �,           �          �        �            �,   �,        O   ����  e�          O   ����  R�          O   ����  ��      �-    V  t-  �-      �     4   �����     $   W  �-  ���                       ( @                      � ߱        /  $  [  .  ���                       @     
                   � ߱        h      	 ��| 	       �      
 ��� 
       �       ���        �       ���                ��         0        ��D         X        ��l             � ߱        4/  $  ]  4.  ���                       �/  $  e  `/  ���                       �                         � ߱        �/  $  f  �/  ���                       �                         � ߱        p1    h   0  |0      �      4   �����                D1                      ��                  h  q                  ��p                       h  0  �       	   ! 	       ,!      
   d! 
       x!         �!        �!         �!        "         H"        \"         �"            � ߱            $  i  �0  ���                       �2    s  �1  2      �"     4   �����"               �2                      ��                  s  |                  ��p                       s  �1  $#      	   \# 	       p#      
   �# 
       �#         �#        $         @$        T$         �$        �$         �$            � ߱            $  t  2  ���                       �4    ~  3  �3      �$     4   �����$               \4                      ��                  ~  �                  d�p                       ~  (3  �%      	   �% 	       &      
   H& 
       \&         �&        �&         �&        �&         ,'        @'         x'            � ߱            $    �3  ���                       6    �  �4   5      �'     4   �����'               �5                      ��                  �  �                  ��p                       �  �4  l(      	   �( 	       �(      
   �( 
       )         <)        P)         �)        �)         �)        �)          *            � ߱            $  �  05  ���                       H8    �  06  �6      4*     4   ����4*               �6                      ��                  �  �                  �p                       �  @6  �7    �  �6  �6  @7  @*     4   ����@*     $  �  7  ���                       `*                        � ߱            $  �  l7  ���                       l*                        � ߱        �7  $  �  �7  ���                       �*                        � ߱            $  �  8  ���                       �*                        � ߱        �B    �  d8  �8      0+     4   ����0+               �9                      ��                  �  �                  ��p                       �  t8  X+     
               d+     
               p+     
               |+     
               �+     
               �+     
               �+     
               �+     
                   � ߱        :  $  �  �8  ���                       �;    �  (:  �:      �+     4   �����+               �:                      ��                  �  �                  D�p                       �  8:  �;    �  �:  �:  8;  �+     4   �����+     $  �  ;  ���                        ,                        � ߱            $  �  d;  ���                       ,                        � ߱            $  �  �;  ���                       ,,                        � ߱        @<  $  �  <  ���                       t,     
                   � ߱        �<  $  �  l<  ���                       �,     
                   � ߱        P=  A  �       6 �<   ��        A �<                                            �,                <=  0=           �,           -        �            =    =    ,>    �  l=  |=  �=  -     4   ����-     $  �  �=  ���                       -     
                   � ߱            $  �   >  ���                       L-     
                   � ߱         ?  A  �       7 �>   ��        B |>  �-                                       X-  d-  p-                �>  �>           |- �- �-          �- �- �-        �            �>   �>    �?    �  ?  �?      �-     4   �����-  .     
               <.     
               x.     
               �.     
                   � ߱            $  �  ,?  ���                       �@    �  �?  @  \@  �.     4   �����.     $  �  0@  ���                       /                        � ߱            $  �  �@  ���                       0/                        � ߱        A  $  �  �@  ���                       d/                        � ߱        dA  $  �  8A  ���                       �/                        � ߱        �A  $  �  �A  ���                       �/                        � ߱        B  $  �  �A  ���                        0                        � ߱        lB  $  �  @B  ���                       40                        � ߱            $  �  �B  ���                       h0                        � ߱        |C  A  �        C   ��        C C                                             �0                hC  \C           �0          �0        �            <C   LC    �G    �  �C  D      �0     4   �����0               $D                      ��                  �  �                  ��p                       �  �C  4D  9   �         $  �  `D  ���                       �0                        � ߱        �0          1        41         �1        �1         �1        �1         L2        `2         �2        �2         3        (3      	   x3 	       �3         �3        �3         @4        T4         �4        �4         5        5         l5        �5         �5        �5      	   46 	       H6         �6        �6         �6        7         `7        t7         �7        �7         (8        <8         �8        �8      	   �8 	       9         T9        h9         �9        �9         :        0:         �:        �:         �:        �:         H;        \;      	   �; 	           � ߱            $  �  �D  ���                         ��                             ��                             ��                              ��        	                   ��                            ����                                =   �         =   �     �H    �H  7      6                  �           �   l       ���[               2  d  �               �q                    O   ����    e�          O   ����    R�          O   ����    ��      �;     
               �;     
                   � ߱        �  $  9  �   ���                             �      8            �      ��                  <  R                 q                �/     <  (      �          ��                            7   ����         
 ��          D           �            p                  6   <        �  
 ��         D           �            p                                                                �  �      
             
                @            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �  $  =  d  ���                       �;     
                   � ߱        l    >  �  �    <     4   ����<     $  >  �  ���                       (<     
                   � ߱            $  ?  @  ���                       4<     
                   � ߱              @  �        h<     4   ����h<               �                      ��                  @  Q                  �o                       @  �        �      4  �[        �      ��                  Y                  ��o                �/     Y    $  �         ��                            7   ����    ;      ��          E     L=   �            \                  6   Y       ; �   ��        E �  L=   �            \                                                        �<  �<  �<  �<  �<  �<  �<  �< 	 = 
 =  0=                             <=          D=                     �   �    |  P  �       ��$                           A   ����    5      ��          F           �            �                  6   Y       5     ��        F         �            �                          *                              <>                h  \           H>          P>        �            <   L    �	  �  �       ��$                           A   ����    0      ��          G           �            H	                  6   Y       0 x	   ��        G l	        �            H	                          *                              X>                �	  �	           d>          l>        �            �	   �	    ,   
  P
       ��$                           A   ����    1      ��          H           �            �
                  6   Y       1 �
   ��        H �
        �            �
                          *                              t>                             �>          �>        �            �
   �
    �  X  �       ��$                           A   ����    %      ��          I     �>   �            �                  6   Y       % 0   ��        I   �>   �            �                          *                              �>  �>                  �  t           �> �>          �> �>        �            L   `        �         ��$                           A   ����    &      ��          J           �            `                  6   Y       & �   ��        J �        �            `                          *                              �>                �  �           ?          ?        �            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �    e  P  `      ?     4   ����?     $   f  �  ���                       �? @         �?             � ߱        �  $  j  �  ���                       �@     
                   � ߱        �@      	 ���@ 	       �@      
 ��A 
       $A       ��8A        LA       ��`A        tA       ���A        �A       ���A        �A       ���A            � ߱          $  l    ���                       h  $  t  <  ���                       �A                        � ߱        �  $  u  �  ���                       �A                        � ߱        L    w  �  X      B     4   ����B                                      ��                  w  �                  �?v                       w  �  LB      	   �B 	       �B      
   �B 
       �B         C        0C         hC        |C         �C        �C          D            � ߱            $  x  h  ���                       �    �  h  �      D     4   ����D               �                      ��                  �  �                  \Rv                       �  x  �D      	   �D 	       �D      
   E 
       (E         `E        tE         �E        �E         �E        F         DF            � ߱            $  �  �  ���                       d    �  �  p      XF     4   ����XF               8                      ��                  �  �                  �Tv                       �    0G      	   hG 	       |G      
   �G 
       �G          H        H         LH        `H         �H        �H         �H            � ߱            $  �  �  ���                       �    �  �  �      �H     4   �����H               �                      ��                  �  �                  Wv                       �  �  �I      	   J 	       $J      
   \J 
       pJ         �J        �J         �J        K         @K        TK         �K            � ߱            $  �    ���                       $    �    �      �K     4   �����K               �                      ��                  �  �                  tYv                       �    t    �  �  �    �K     4   �����K     $  �  �  ���                       �K                        � ߱            $  �  H  ���                       �K                        � ߱        �  $  �  �  ���                       �K                        � ߱            $  �  �  ���                       @L                        � ߱        �#    �  @  �      �L     4   �����L 	              �                      ��             
     �  �                  (Zv                       �  P  �L     
               �L     
               �L     
               �L     
               �L     
                M     
               M     
               M     
                   � ߱        �  $  �  �  ���                       �    �    �      $M     4   ����$M 
              �                      ��             
     �  �                  �Zv                       �    l    �  �  �    LM     4   ����LM     $  �  �  ���                       lM                        � ߱            $  �  @  ���                       xM                        � ߱            $  �  �  ���                       �M                        � ߱          $  �  �  ���                       �M     
                   � ߱        t  $  �  H  ���                       N     
                   � ߱        ,  A  �       6 �   ��        K �                                            XN                             dN          lN        �            �   �        �  H  X  �  tN     4   ����tN     $  �  �  ���                       |N     
                   � ߱            $  �  �  ���                       �N     
                   � ߱        �  A  �       7 p   ��        L X  O                                       �N  �N  �N                �  �           �N �N O          �N  O O        �            �   �    �     �  �  �       dO     4   ����dO lO     
               �O     
               �O     
                P     
                   � ߱            $  �     ���                       �!    �  �   �   8!  \P     4   ����\P     $  �  !  ���                       |P                        � ߱            $  �  d!  ���                       �P                        � ߱        �!  $  �  �!  ���                       �P                        � ߱        @"  $  �  "  ���                       Q                        � ߱        �"  $  �  l"  ���                       8Q                        � ߱        �"  $  �  �"  ���                       lQ                        � ߱        H#  $  �  #  ���                       �Q                        � ߱            $  �  t#  ���                       �Q                        � ߱        T*    �  �#  8$      R     4   ����R               H$                      ��                  �  �                  �~v                       �  �#  |&    �  d$  �$      0R     4   ����0R               �$                      ��                  �  �                   v                       �  t$  �%    �  %  %  t%  XR     4   ����XR     $  �  H%  ���                       xR                        � ߱            $  �  �%  ���                       �R                        � ߱        $&  $  �  �%  ���                       �R                        � ߱            $  �  P&  ���                       �R                        � ߱         (    �  �&  '      HS     4   ����HS               $'                      ��                  �  �                  �v                       �  �&        �  @'  P'  �'  pS     4   ����pS     $  �  |'  ���                       �S                        � ߱            $  �  �'  ���                       �S                        � ߱        X(  $  �  ,(  ���                       �S                        � ߱         )  A  �       < �(   ��        M �(  <T                                       T  T                  )   )           T ,T          $T 4T        �            �(   �(    �)    �  <)  L)  �)  lT     4   ����lT     $  �  x)  ���                       tT                        � ߱            $  �  �)  ���                       �T                        � ߱            $  �  (*  ���                       �T                        � ߱        +  A  �       �*   ��        N �*                                             U                �*  �*           U          $U        �            �*   �*    </    �  (+  �+      ,U     4   ����,U               �+                      ��                  �  �                  �v                       �  8+  �+  9   �         $  �  �+  ���                       8U                        � ߱        DU         �U        �U         �U        V         \V        pV         �V        �V         $W        8W         �W        �W      	   �W 	        X         PX        dX         �X        �X         Y        ,Y         |Y        �Y         �Y        �Y         DZ        XZ      	   �Z 	       �Z         [         [         p[        �[         �[        �[         8\        L\         �\        �\          ]        ]      	   d] 	       x]         �]        �]         ,^        @^         �^        �^         �^        _         X_        l_         �_        �_      	    ` 	           � ߱            $  �  ,  ���                       4`     
               H`     
                   � ߱            $  N  h/  ���                             S  �/  t0      T`     4   ����T`               �0                      ��                  S  a                  T�v                       S  0         1      �9  �[      t9  \9      ��                  l  ,  �9              �v                       l  �0  �2  ,1  |1       ��                            7   ����    ;      ��          O     8a   �            �1                  6   l       ; 82   ��        O �1  8a   �            �1                                                        |`  �`  �`  �`  �`  �`  �`  �` 	 �` 
  a  a                �2  t2           (a          0a                     T2   d2    �3  �2  3       ��$                           A   ����    5      ��          P           �            `3                  6   l       5 �3   ��        P �3        �            `3                          *                              (b                �3  �3           4b          <b        �            �3   �3    D5  4  h4       ��$                           A   ����    0      ��          Q           �            �4                  6   l       0 �4   ��        Q �4        �            �4                          *                              Db                05  $5           Pb          Xb        �            5   5    �6  p5  �5       ��$                           A   ����    1      ��          R           �            6                  6   l       1 @6   ��        R 46        �            6                          *                              `b                �6  |6           lb          tb        �            \6   l6    8  �6  7       ��$                           A   ����    %      ��          S     �b   �            h7                  6   l       % �7   ��        S �7  �b   �            h7                          *                              |b  �b                  �7  �7           �b �b          �b �b        �            �7   �7        08  �8       ��$                           A   ����    &      ��          T           �            �8                  6   l       &  9   ��        T �8        �            �8                          *                              �b                H9  <9           �b          �b        �            9   ,9        O   ����  e�          O   ����  R�          O   ����  ��      (:    x  �9  �9       c     4   ���� c     $   y  �9  ���                       �c @         lc             � ߱        T;  $  }  T:  ���                       �d     
                   � ߱        �d      	 ���d 	       �d      
 ���d 
       e       ��$e        8e       ��Le        `e       ��te        �e       ���e        �e       ���e            � ߱        �;  $    �:  ���                       �;  $  �  �;  ���                       �e                        � ߱        0<  $  �  <  ���                       �e                        � ߱        �=    �  L<  �<      �e     4   �����e               �=                      ��                  �  �                  $�v                       �  \<  8f      	   pf 	       �f      
   �f 
       �f         g        g         Tg        hg         �g        �g         �g            � ߱            $  �  �<  ���                       H?    �  �=  T>       h     4   ���� h               ?                      ��                  �  �                  `�v                       �  �=  |h      	   �h 	       �h      
    i 
       i         Li        `i         �i        �i         �i        �i         0j            � ߱            $  �  d>  ���                       �@    �  d?  �?      Dj     4   ����Dj               �@                      ��                  �  �                  лv                       �  t?  k      	   Tk 	       hk      
   �k 
       �k         �k         l         8l        Ll         �l        �l         �l            � ߱            $  �  �?  ���                       `B    �  �@  lA      �l     4   �����l               4B                      ��                  �  �                  �v                       �   A  �m      	   �m 	       n      
   Hn 
       \n         �n        �n         �n        �n         ,o        @o         xo            � ߱            $  �  |A  ���                       �D    �  |B  �B      �o     4   �����o               C                      ��                  �  �                  �v                       �  �B  �C    �  $C  4C  �C  �o     4   �����o     $  �  `C  ���                       �o                        � ߱            $  �  �C  ���                       �o                        � ߱        <D  $  �  D  ���                       �o                        � ߱            $  �  hD  ���                       ,p                        � ߱        O    �  �D  ,E      �p     4   �����p               ,F                      ��                  �  �                  ��v                       �  �D  �p     
               �p     
               �p     
               �p     
               �p     
               �p     
               �p     
               q     
                   � ߱        XF  $  �  <E  ���                       4H    �  tF  �F      q     4   ����q                G                      ��                  �  �                  T�v                       �  �F  �G    �  G  ,G  �G  8q     4   ����8q     $  �  XG  ���                       Xq                        � ߱            $  �  �G  ���                       dq                        � ߱            $  �  H  ���                       �q                        � ߱        �H  $  �  `H  ���                       �q     
                   � ߱        �H  $  �  �H  ���                       r     
                   � ߱        �I  A  �       6 @I   ��        U 4I                                            Dr                �I  |I           Pr          Xr        �            \I   lI    xJ    �  �I  �I   J  `r     4   ����`r     $  �  �I  ���                       hr     
                   � ߱            $  �  LJ  ���                       �r     
                   � ߱        LK  A  �       7 �J   ��        V �J  s                                       �r  �r  �r                8K  ,K           �r �r �r          �r �r �r        �            �J   K    $L    �  hK  �K      Ps     4   ����Ps Xs     
               �s     
               �s     
               t     
                   � ߱            $  �  xK  ���                        M    �  @L  PL  �L  Ht     4   ����Ht     $  �  |L  ���                       ht                        � ߱            $  �  �L  ���                       �t                        � ߱        XM  $  �  ,M  ���                       �t                        � ߱        �M  $  �  �M  ���                       �t                        � ߱        N  $  �  �M  ���                       $u                        � ߱        `N  $  �  4N  ���                       Xu                        � ߱        �N  $  �  �N  ���                       �u                        � ߱            $  �  �N  ���                       �u                        � ߱        �U    �  ,O  �O      �u     4   �����u               �O                      ��                  �                    l�v                       �  <O  �Q    �  �O  PP      v     4   ����v               `P                      ��                  �  �                  ��v                       �  �O  <Q    �  |P  �P  �P  Dv     4   ����Dv     $  �  �P  ���                       dv                        � ߱            $  �  Q  ���                       pv                        � ߱        �Q  $  �  hQ  ���                       �v                        � ߱            $  �  �Q  ���                       �v                        � ߱        pS    �  R  �R      4w     4   ����4w               �R                      ��                  �  �                  ��v                       �  R        �  �R  �R  S  \w     4   ����\w     $  �  �R  ���                       |w                        � ߱            $  �  DS  ���                       �w                        � ߱        �S  $     �S  ���                       �w                        � ߱        �T  A         < ,T   ��        W T  (x                                       �w  �w                  |T  pT           x x          x  x        �            HT   \T    lU      �T  �T  U  Xx     4   ����Xx     $    �T  ���                       `x                        � ߱            $    @U  ���                       �x                        � ߱            $    �U  ���                       �x                        � ߱        |V  A  
        V   ��        X V                                             �x                hV  \V           y          y        �            <V   LV    �Z      �V  W      y     4   ����y               $W                      ��                                      ��v                         �V  4W  9            $    `W  ���                       $y                        � ߱        0y         �y        �y         �y        �y         Hz        \z         �z        �z         {        ${         t{        �{      	   �{ 	       �{         <|        P|         �|        �|         }        }         h}        |}         �}        �}         0~        D~      	   �~ 	       �~         �~                 \        p         �        �         $�        8�         ��        ��         �         �      	   P� 	       d�         ��        ȁ         �        ,�         |�        ��         ��        �         D�        X�         ��        ��      	   � 	           � ߱            $    �W  ���                         ��                             ��                             ��                              ��        	                   ��                            ����                                =   ,         =        �[    �[  <  �[  7      6                             �   l       ��                j  �  �               �w                    O   ����    e�          O   ����    R�          O   ����    ��            0      �        \  D      ��                  t  �  t              �)q                       t  �   �  \  �       ��                            7   ����    =      ��          Y     T�   �            �                  6   t       = 8   ��        Y    T�   �            �                                                         �  ,�  8�                �  t           D�          L�                     T   d    �  �         ��$                           A   ����    0      ��          Z           �            `                  6   t       0 �   ��        Z �        �            `                          *                              ��                �  �           ��          ��        �            �   �          h       ��$                           A   ����    5      ��          [           �            �                  6   t       5 �   ��        [ �        �            �                          *                              Ą                0  $           Є          ؄        �                       O   ����  e�          O   ����  R�          O   ����  ��          y  �  �      ��     4   ������     $   z  �  ���                       `� @         L�             � ߱        <  $  ~  <  ���                       8�     
                   � ߱        `�      	 ��t� 	       ��      
 ���� 
       ��       ��Ć        ؆       ���         �       ���        (�       ��<�        P�       ��d�            � ߱        h  $  �  h  ���                       �  $  �  �  ���                       x�                        � ߱          $  �  �  ���                       ��                        � ߱        �	    �  4  �      ��     4   ������               x	                      ��                  �  �                  8;w                       �  D  ؇      	   � 	       $�      
   \� 
       p�         ��        ��         �        �         @�        T�         ��            � ߱            $  �  �  ���                       0    �  �	  <
      ��     4   ������                                     ��                  �  �                  �=w                       �  �	  �      	   T� 	       h�      
   �� 
       ��         �         �         8�        L�         ��        ��         Ћ            � ߱            $  �  L
  ���                       �    �  L  �      �     4   �����               �                      ��                  �  �                  �?w                       �  \  ��      	   � 	       �      
   @� 
       T�         ��        ��         ؍        �         $�        8�         p�            � ߱            $  �  �  ���                       H    �  �  T      ��     4   ������                                     ��                  �  �                  �Rw                       �  �  d�      	   �� 	       ��      
   � 
       ��         4�        H�         ��        ��         ̐        ��         �            � ߱            $  �  d  ���                       |    �  d  �      ,�     4   ����,�               �                      ��                  �  �                  Uw                       �  t  �    �      t  8�     4   ����8�     $  �  H  ���                       X�                        � ߱            $  �  �  ���                       d�                        � ߱        $  $  �  �  ���                       ��                        � ߱            $  �  P  ���                       ̑                        � ߱        4  A  �       �   ��        \ �                                             (�                              4�          <�        �            �       d    �  P  �      D�     4   ����D�               �                      ��                  �  �                  �Vw                       �  `  �  9   �         $  �    ���                       P�                        � ߱        \�         ��        ��         �        $�         t�        ��         ؓ        �         <�        P�         ��        ��      	   � 	       �         h�        |�         ̕        ��         0�        D�         ��        ��         ��        �         \�        p�      	   �� 	       ԗ         $�        8�         ��        ��         �         �         P�        d�         ��        ș         �        ,�      	   |� 	       ��         ��        ��         D�        X�         ��        ��         �         �         p�        ��         Ԝ        �      	   8� 	           � ߱            $  �  D  ���                         ��                             ��                              ��        	                  ����                                =   �                           �           �   l       ��h               �  A  �               @tw                    O   ����    e�          O   ����    R�          O   ����    ��      0  �   �  	         @      �  X      �  �      ��                  �  :  �              ��w                L	     �  �       l  �       ��                            7   ����         ��          ]     X�   �                              6   �       <   ��        ] 0  X�   �                                                                    L�                �  x                                   @            X   h        O   ����  e�          O   ����  R�          O   ����  ��        A  �      	 �   ��        ^ 0  X�                                        t�  ��  ��  ��  ��  ��  ��  ȝ 	 ԝ 
 ��  �  ��  �  �  �  (�  4�  @�  L�                �  �                                   @            �   �    @         0      ��     4   ������     9     	   �  �      	 �  �         �     	               �     	                �     	               @�     	               `�     	               ��     	               ��     	               ��     	               �     	                �     	                �     	               @�     	                 `�     	 !       !       ��     	 "       "       ��     	 #       #       ��     	 $       $       �     	 %       %        �     	 &       &        �     	 '       '       @�     	 (       (       `�     	 )       )       ��     	 *       *       ��     	 +       +       ��     	 ,       ,       �     	 -       -        �     	 .       .        �     	 /       /       @�     	 0       0       `�     	 1       1       ��     	 2       2           � ߱            $    `  ���                                                                                                          
 
 
                                      	 	 	                               :   9                   \	      �
  H      �
  �
      ��                  <  ?  �
              4<|                       <  �      �	  �	       ��                            7   ����    	      ��          _           �            (
                  6   <       	 L
   ��         _           �            (
                                                                �
  �
                                   @            h
   x
        O   ����  e�          O   ����  R�          O   ����  ��         9   =         �   >  	         1                                                                                          $ $ $      0 0 0      * * *              # # #      / / /      ) ) )                                              
 
 
                                         & & &      2 2 2      , , ,              % % %      1 1 1      + + +                      	 	 	                                      " " "      . . .      ( ( (              ! ! !      - - -      ' ' '                      >  �          �  �    �                                        >     ��                             ��                             ��                            ����                                =   ?         =   :  	       	                             �   l       ��$%               G  �  �               8D|                    O   ����    e�          O   ����    R�          O   ����    ��            0      �	  %      �	  �	      ��                  R  �  �	              �F|                       R  �   �  \  �       ��                            7   ����	    ?      ��          `     �   �            �                  6   R  	     ? P   ��        `    �   �            �                                                        ��  ��  ��  ̤  ؤ  �   �                �  �           �          �             	        l  	 |      �  (       ��$                           A   ����    0      ��          a           �            x                  6   R       0 �   ��        a �        �            x                          *                              ��                �  �           ��          ȥ        �            �   �    �  0  �       ��$                           A   ����
    '      ��          b     `�   �            �                  6   R  
     ' ,   ��        b �  `�   �            �                          *                              Х  ܥ  �  ��   �  �  (�  D�                  t  h           P�          X�        �   	 
       	 H  
 X    �  �         ��$                           A   ����    1      ��          c           �            T                  6   R       1 �   ��        c x        �            T                          *                              �                �  �           $�          ,�        �            �   �    H    \       ��$                           A   ����    %      ��          d     l�   �            �                  6   R       % �   ��        d �  l�   �            �                          *                              4�  @�                  4  (           L� \�          T� d�        �                        t  �       ��$                           A   ����    5      ��          e           �            	                  6   R       5 D	   ��        e 8	        �            	                          *                              ��                �	  �	           ��          ��        �            `	   p	        O   ����  e�          O   ����  R�          O   ����  ��      l
    ]  
  
      ��     4   ������     $   ^  @
  ���                       8� @         $�             � ߱        �  $  b  �
  ���                       P�     
                   � ߱        x�      	 ���� 	       ��      
 ���� 
       ȩ       ��ܩ        �       ���        �       ��,�        @�       ��T�        h�       ��|�            � ߱        �  $  d  �
  ���                         $  l  �  ���                       ��                        � ߱        t  $  m  H  ���                       ��                        � ߱            o  �        ��     4   ������               �                      ��                  o  x                   a|                       o  �  �         (�        <�      	   t� 	       ��      
   �� 
       ԫ         �         �         X�        l�         ��        ��         �            � ߱            $  p    ���                       �    z  8  �      �     4   �����               �                      ��                  z  �                  �c|                       z  H  ��         ��        ̭      	   � 	       �      
   P� 
       d�         ��        ��         �        ��         4�        H�         ��            � ߱            $  {  �  ���                       l    �  �  \      ��     4   ������               @                      ��                  �  �                  �v|                       �  �  l�         ��        ��      	   � 	       �      
   <� 
       P�         ��        ��         Ա        �          �        4�         l�            � ߱            $  �  l  ���                           �  �        ��     4   ������               �                      ��                  �  �                  ��|                       �  �  `�         ��        ��      	   � 	       ��      
   0� 
       D�         |�        ��         ȴ        ܴ         �        (�         `�            � ߱            $  �    ���                       �    �  0  �      t�     4   ����t�               �                      ��                  �  �                  �|                       �  @  �    �  �  �  @  ��     4   ������     $  �    ���                       ��                        � ߱            $  �  l  ���                       �                        � ߱            $  �  �  ���                       D�                        � ߱        �    �    �      ��     4   ������               �                      ��                  �  �                  �|                       �    X    �  �  0  �  ȶ     4   ����ȶ               @                      ��                  �  �                  ��|                       �  �      �  \  l  �   �     4   ���� �     $  �  �  ���                       @�                        � ߱            $  �  �  ���                       ��                        � ߱        t  $  �  H  ���                       �                        � ߱        �  $  �  �  ���                       @�                        � ߱        $  $  �  �  ���                       ��                        � ߱            $  �  P  ���                       и                        � ߱        	              �                      ��                  �  �                  $�|                       �  |  �    �    �      �     4   ����� 
              �                      ��             
     �  �                  ��|                       �  $  |    �  �  �  $  ,�     4   ����,�     $  �  �  ���                       L�                        � ߱            $  �  P  ���                       ��                        � ߱            $  �  �  ���                       �                        � ߱              �  �  l      L�     4   ����L�               |                      ��                  �  �                  L�|                       �           �  �  �     t�     4   ����t�     $  �  �  ���                       ��                        � ߱            $  �  ,  ���                       ��                        � ߱          A  �       8 �   ��        f �                                            �                �  �           ��          ��        �            �   �    �    �  ,  <  �  �     4   �����     $  �  h  ���                       �                        � ߱            $  �  �  ���                       h�                        � ߱        �  A  �       ( H   ��        g <                                            ��                �  �           ��          Ȼ        �            d   t          �  �  �  (  л     4   ����л     $  �  �  ���                       ػ                        � ߱            $  �  T  ���                       L�                        � ߱        8   A  �       �   ��        h �                                             ��                $               ��          ��        �            �        h$    �  T   �       ��     4   ������               �                       ��                  �  �                  h�|                       �  d   �   9   �         $  �  !  ���                       ��                        � ߱        ̼         �        0�         ��        ��         �        ��         H�        \�         ��        ��         �        $�      	   t� 	       ��         ؿ        �         <�        P�         ��        ��         �        �         h�        |�         ��        ��      	   0� 	       D�         ��        ��         ��        �         \�        p�         ��        ��         $�        8�         ��        ��      	   �� 	        �         P�        d�         ��        ��         �        ,�         |�        ��         ��        ��         D�        X�      	   �� 	           � ߱            $  �  H!  ���                         ��                             ��                              ��        	                  ����                                =   �     ,%    4%  (      8                             �   l       ���"               �  �   �               �}                    O   ����    e�          O   ����    R�          O   ����    ��            0      `  �"      0        ��                  �  �   H              �&}                       �  �   �  \  �       ��                            7   ����    @      ��          i     �   �            �                  6   �       @ L   ��        i    �   �            �                                                        ��  ��  ��  ��  ��  ��                  �  �           �          �                     h   x       �  $       ��$                           A   ����    0      ��          j           �            t                  6   �       0 �   ��        j �        �            t                          *                              ��                �  �           ��          ��        �            �   �    X  ,  |       ��$                           A   ����    1      ��          k           �            �                  6   �       1 �   ��        k �        �            �                          *                              ��                D  8           ��          �        �               (    �  �  �       ��$                           A   ����    %      ��          l     D�   �            $                  6   �       % \   ��        l H  D�   �            $                          *                              �  �                  �  �           $� 4�          ,� <�        �            x   �        �  <       ��$                           A   ����    5      ��          m           �            �                  6   �       5 �   ��        m �        �            �                          *                              t�                  �           ��          ��        �            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �       |  �      ��     4   ������     $      �  ���                       � @         ��             � ߱        
  $  	   	  ���                       (�     
                   � ߱        P�      	 ��d� 	       x�      
 ���� 
       ��       ����        ��       ����        ��       ���        �       ��,�        @�       ��T�            � ߱        <
  $     <	  ���                       �
  $     h
  ���                       h�                        � ߱        �
  $     �
  ���                       t�                        � ߱        x         �      ��     4   ������               L                      ��                                        �?}                            ��      	    � 	       �      
   L� 
       `�         ��        ��         ��        ��         0�        D�         |�            � ߱            $     �  ���                           !   �        ��     4   ������               �                      ��                  !   *                   �R}                       !   �  �      	   D� 	       X�      
   �� 
       ��         ��        ��         (�        <�         t�        ��         ��            � ߱            $  "      ���                       �    ,      �      ��     4   ������               d                      ��                  ,   5                   lU}                       ,   0  ��      	   �� 	       ��      
   0� 
       D�         |�        ��         ��        ��         �        (�         `�            � ߱            $  -   �  ���                           7   �  (      t�     4   ����t�               �                      ��                  7   @                   `W}                       7   �  T�      	   �� 	       ��      
   �� 
       ��         $�        8�         p�        ��         ��        ��         �            � ߱            $  8   8  ���                       �    K   8  �      �     4   �����               �                      ��                  K   O                   j}                       K   H  �    L   �  �  H  (�     4   ����(�     $  L     ���                       H�                        � ߱            $  M   t  ���                       ��                        � ߱            $  N   �  ���                       ��                        � ߱        (    P     �      H�     4   ����H�               �                      ��             	     P   b                   �j}                       P   $  |    Q   �  8      p�     4   ����p�               H                      ��                  Q   U                   Dk}                       Q   �  $    R   d  t  �  ��     4   ������     $  R   �  ���                       ��                        � ߱            $  S   �  ���                        �                        � ߱            $  T   P  ���                       \�                        � ߱             V   �        ��     4   ������ 	              $                      ��             	     V   Y                   �k}                       V   �        W   @  P  �  ��     4   ������     $  W   |  ���                        �                        � ߱            $  X   �  ���                        �                        � ߱        �  A  Z        8 \   ��        n P                                            T�                �  �           `�          h�        �            x   �    �    [   �  �  <  p�     4   ����p�     $  \     ���                       |�                        � ߱            $  ]   h  ���                       ��                        � ߱        L  A  ^        ( �   ��        o �                                            �                8  ,           �           �        �                         _   h  x  �  (�     4   ����(�     $  `   �  ���                       0�                        � ߱            $  a   �  ���                       ��                        � ߱        @    c   D  �      ��     4   ������ 
              �                      ��             
     c   j                   �n}                       c   T  �    d   �  �  T  ��     4   ������     $  d   (  ���                       �                        � ߱            $  e   �  ���                       �                        � ߱        d  A  f        )    ��        p �                                            4�                P  D           @�          H�        �            $   4          g   �  �  �  P�     4   ����P�     $  h   �  ���                       X�                        � ߱            $  i     ���                       ��                        � ߱        �  A  l        �   ��        q �                                             ��                �  �           ��          ��        �            �   �    ("    m     �      ��     4   ������               �                      ��                  m   p                   �p}                       m   $  �  9   n          $  o   �  ���                       ��                        � ߱        ��         D�        X�         ��        ��         �         �         p�        ��         ��        ��         8�        L�      	   �� 	       ��          �        �         d�        x�         ��        ��         ,�        @�         ��        ��         ��        �      	   X� 	       l�         ��        ��          �        4�         ��        ��         ��        ��         L�        `�         ��        ��      	   � 	       (�         x�        ��         ��        ��         @�        T�         ��        ��         �        �         l�        ��      	   �� 	           � ߱            $  q     ���                         ��                             ��                              ��        	                  ����                                =   �      �"    �"  )  �"  (      8                  �           �   l       ���M               �   �"  �               4�~                    O   ����    e�          O   ����    R�          O   ����    ��      ��     
               ��     
                   � ߱        �  $  �   �   ���                             �      8            �      ��                  �   �!                 ��~                �(     �   (      �          ��                            7   ����         
 ��          r           �            p                  6   �         �  
 ��         r           �            p                                                                �  �      
             
                @            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �  $  �   d  ���                       �     
                   � ߱        l    �   �  �    ,�     4   ����,�     $  �   �  ���                       L�     
                   � ߱            $  �   @  ���                       X�     
                   � ߱              �   �        ��     4   ������               �                      ��                  �   �!                  ��~                       �   �        �      �  �M      �  t      ��                  �   i!  �              $�~                �(     �       �         ��                            7   ����
    A      ��          s     <�   �            \                  6   �   
     A �   ��        s �  <�   �            \                                                        ��  ��  ��  ��  ��  ��  �   �                     �           ,�          4�            	 
       	 �  
 �    l  @  �       ��$                           A   ����    0      ��          t           �            �                  6   �        0    ��        t         �            �                          *                              ��                X  L           �          �        �            ,   <    �	  �  �       ��$                           A   ����    &      ��          u           �            8	                  6   �        & h	   ��        u \	        �            8	                          *                              �                �	  �	            �          (�        �            �	   �	      �	  @
       ��$                           A   ����    )      ��          v           �            �
                  6   �        ) �
   ��        v �
        �            �
                          *                              0�                  �
           <�          D�        �            �
   �
        H  �       ��$                           A   ����    5      ��          w           �            �                  6   �        5    ��        w         �            �                          *                              L�                `  T           X�          `�        �            4   D        O   ����  e�          O   ����  R�          O   ����  ��      @    �   �  �      h�     4   ����h�     $   �     ���                       �� @         ��             � ߱        l  $  �   l  ���                       (�     
                   � ߱        P�      	 ��d� 	       x�      
 ���� 
       ��       ����        ��       ����        ��       ���        �       ��,�        @�       ��T�            � ߱        �  $  �   �  ���                       �  $  �   �  ���                       h�                        � ߱        H  $  �     ���                       t�                        � ߱        �    �   d  �      ��     4   ������               �                      ��                  �   �                   |�~                       �   t  ��      	    � 	       �      
   L� 
       `�         ��        ��         ��        ��         0�        D�         |�            � ߱            $  �   �  ���                       `    �   �  l      ��     4   ������               4                      ��                  �   �                   ��~                       �      �      	   D� 	       X�      
   �� 
       ��         ��        ��         (�        <�         t�        ��         ��            � ߱            $  �   |  ���                       �    �   |  �      ��     4   ������               �                      ��                  �   �                   t�~                       �   �  ��      	   �� 	       ��      
   0� 
       D�         |�        ��         ��        ��         �        (�         `�            � ߱            $  �     ���                       x    �     �      t�     4   ����t�               L                      ��                  �   !                  ��~                       �     T�      	   �� 	       ��      
   �� 
       ��         $�        8�         p�        ��         ��        ��         �            � ߱            $  �   �  ���                       T    !  �        �     4   �����                                      ��                  !  !                   �~                       !  �  �    !  <  L  �  (�     4   ����(�     $  !  x  ���                       H�                        � ߱            $  !  �  ���                       ��                        � ߱            $  !  (  ���                       ��                        � ߱        �!    !  p  �      H�     4   ����H� 	              �                      ��             
     !  @!                  ��~                       !  �  p�     
               |�     
               ��     
               ��     
               ��     
               ��     
               ��     
               ��     
                   � ߱          $  !  �  ���                       �    !  4  �      ��     4   ������ 
              �                      ��             
     !   !                  P�~                       !  D  �    !  �  �  D  ��     4   ������     $  !    ���                       �                        � ߱            $  !  p  ���                       $�                        � ߱            $  !  �  ���                       D�                        � ߱        L  $  "!     ���                       ��     
                   � ߱        �  $  $!  x  ���                       ��     
                   � ߱        \  A  &!       6     ��        x �                                            �                H  <           �          �        �               ,    8    '!  x  �  �   �     4   ���� �     $  '!  �  ���                       (�     
                   � ߱            $  (!    ���                       d�     
                   � ߱          A  *!       7 �   ��        y �  ��                                       p�  |�  ��                �  �           �� �� ��          �� �� ��        �            �   �    �    .!  (  �      �     4   ����� �     
               T�     
               ��     
               ��     
                   � ߱            $  /!  8  ���                       �    7!       h  �     4   �����     $  7!  <  ���                       (�                        � ߱            $  8!  �  ���                       H�                        � ߱           $  :!  �  ���                       |�                        � ߱        p   $  ;!  D   ���                       ��                        � ߱        �   $  <!  �   ���                       ��                        � ߱         !  $  =!  �   ���                       �                        � ߱        x!  $  >!  L!  ���                       L�                        � ߱            $  ?!  �!  ���                       ��                        � ߱        T#    B!  �!  h"      ��     4   ������               x"                      ��                  B!  E!                  ��~                       B!  �!        C!  �"  �"  �"  ��     4   ������     $  C!  �"  ���                       ��                        � ߱            $  D!  (#  ���                       D�                        � ߱        $  A  G!       �#   ��        z �#                                             ��                �#  �#           ��          ��        �            �#   �#    <(    H!  ($  �$      ��     4   ������               �$                      ��                  H!  K!                  ��~                       H!  8$  �$  9   I!         $  J!  �$  ���                       ��                        � ߱        ��         $�        8�         ��        ��         ��         �         P�        d�         ��        ��                  ,       	   |  	       �          �         �          D        X         �        �                           p        �         �        �      	   8 	       L         �        �                           d        x         �        �         ,        @         �        �      	   � 	                X        l         �        �                  4         �        �         �        �         L        `      	   � 	           � ߱            $  L!  %  ���                       �     
               �     
                   � ߱            $  �!  h(  ���                             �!  �(  t)      �     4   �����               �)                      ��                  �!  �"                                         �!  )         *      ,1  �M      �0  �0      ��                  �!  h"  1              �                       �!  �)  �+  ,*  |*       ��                            7   ����
    A      ��          {     �	   �            �*                  6   �!  
     A (+   ��        { �*  �	   �            �*                                                        	  	  ,	  8	  D	  P	  \	  x	                  p+  d+           �	          �	            	 
       	 D+  
 T+    �,  �+   ,       ��$                           A   ����    0      ��          |           �            P,                  6   �!       0 �,   ��        | t,        �            P,                          *                              P
                �,  �,           \
          d
        �            �,   �,    4.  -  X-       ��$                           A   ����    &      ��          }           �            �-                  6   �!       & �-   ��        } �-        �            �-                          *                              l
                 .  .           x
          �
        �            �-   .    �/  `.  �.       ��$                           A   ����    )      ��          ~           �             /                  6   �!       ) 0/   ��        ~ $/        �             /                          *                              �
                x/  l/           �
          �
        �            L/   \/        �/  0       ��$                           A   ����    5      ��                     �            X0                  6   �!       5 �0   ��         |0        �            X0                          *                              �
                �0  �0           �
          �
        �            �0   �0        O   ����  e�          O   ����  R�          O   ����  ��      �1    �!  H1  X1      �
     4   �����
     $   �!  �1  ���                       @ @         ,             � ߱        �2  $  �!  �1  ���                       �     
                   � ߱        �      	 ��� 	       �      
 ��� 
       �       ��                ��4        H       ��\        p       ���        �       ���            � ߱        3  $  �!  2  ���                       `3  $  �!  43  ���                       �                        � ߱        �3  $  �!  �3  ���                       �                        � ߱        D5    �!  �3  P4      �     4   �����               5                      ��                  �!  �!                  �(                       �!  �3         	   X 	       l      
   � 
       �         �                 <        P         �        �         �            � ߱            $  �!  `4  ���                       �6    �!  `5  �5      �     4   �����               �6                      ��                  �!  �!                  �;                       �!  p5  d      	   � 	       �      
   � 
       �         4        H         �        �         �        �                     � ߱            $  �!  �5  ���                       \8    �!  �6  h7      ,     4   ����,               08                      ��                  �!  �!                  �=                       �!  �6        	   < 	       P      
   � 
       �         �        �                  4         l        �         �            � ߱            $  �!  x7  ���                       �9    �!  x8  �8      �     4   �����               �9                      ��                  �!  "                  <@                       �!  �8  �      	   � 	       �      
   0 
       D         |        �         �        �                 (         `            � ߱            $  �!  9  ���                       �;    "  :  �:      t     4   ����t               �:                      ��                  "  "                  C                       "  :  l;    "  �:  �:  ;  �     4   �����     $  "  �:  ���                       �                        � ߱            $  "  @;  ���                       �                        � ߱            $  "  �;  ���                       D                        � ߱        @F    "  �;  \<      �     4   �����               \=                      ��                  "  ?"                  hC                       "  �;  �     
               �     
               �     
               �     
               �     
                    
                    
                    
                   � ߱        �=  $  "  l<  ���                       d?    "  �=   >      (     4   ����(               0>                      ��                  "  "                  �C                       "  �=  ?    "  L>  \>  �>  P     4   ����P     $  "  �>  ���                       p                        � ߱            $  "  �>  ���                       |                        � ߱            $  "  8?  ���                       �                        � ߱        �?  $  !"  �?  ���                       �     
                   � ߱        @  $  #"  �?  ���                             
                   � ߱        �@  A  %"       6 p@   ��        � d@                                            \                �@  �@           h          p        �            �@   �@    �A    &"  �@  �@  PA  x     4   ����x     $  &"  $A  ���                       �     
                   � ߱            $  '"  |A  ���                       �     
                   � ߱        |B  A  )"       7 B   ��        � �A                                         �  �  �                hB  \B           � �           �          �            ,B   DB    TC    -"  �B  (C      h     4   ����h p     
               �     
               �     
               $     
                   � ߱            $  ."  �B  ���                       0D    6"  pC  �C  �C  `     4   ����`     $  6"  �C  ���                       �                        � ߱            $  7"  D  ���                       �                        � ߱        �D  $  9"  \D  ���                       �                        � ߱        �D  $  :"  �D  ���                                               � ߱        8E  $  ;"  E  ���                       <                        � ߱        �E  $  <"  dE  ���                       p                        � ߱        �E  $  ="  �E  ���                       �                        � ߱            $  >"  F  ���                       �                        � ߱        �G    A"  \F  �F           4   ����               �F                      ��                  A"  D"                  �E                       A"  lF        B"  G  G  lG  4     4   ����4     $  B"  @G  ���                       T                        � ߱            $  C"  �G  ���                       �                        � ߱        |H  A  F"        H   ��        � H                                             �                hH  \H                             �            <H   LH    �L    G"  �H  I           4   ����               $I                      ��                  G"  J"                  G                       G"  �H  4I  9   H"         $  I"  `I  ���                                                � ߱        ,         |        �         �        �         D         X          �         �          !         !         p!        �!      	   �! 	       �!         8"        L"         �"        �"          #        #         d#        x#         �#        �#         ,$        @$      	   �$ 	       �$         �$        %         X%        l%         �%        �%          &        4&         �&        �&         �&        �&      	   L' 	       `'         �'        �'         (        ((         x(        �(         �(        �(         @)        T)         �)        �)      	   * 	           � ߱            $  K"  �I  ���                         ��                             ��                             ��                              ��        	                   ��                            ����                                =   h"         =   i!     �M    �M  7      6                             �   l       ��               �"  -#  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��            0      �  �      �  �      ��                  �"  +#  �              �~                       �"  �   �  \  �       ��                            7   ����    =      ��          �     \*   �            �                  6   �"       = @   ��        �    \*   �            �                                                        *  (*  4*  @*                  �  |           L*          T*                     \   l    �  �         ��$                           A   ����    0      ��          �           �            h                  6   �"       0 �   ��        � �        �            h                          *                              �*                �  �           �*          �*        �            �   �    L     p       ��$                           A   ����    )      ��          �           �            �                  6   �"       ) �   ��        � �        �            �                          *                              �*                8  ,           �*          +        �                       x  �       ��$                           A   ����    5      ��          �           �                              6   �"       5 H   ��        � <        �                                      *                              +                �  �           +           +        �            d   t        O   ����  e�          O   ����  R�          O   ����  ��      p    �"          (+     4   ����(+     $   �"  D  ���                       �+ @         �+             � ߱        �  $  �"  �  ���                       �,     
                   � ߱        �,      	 ���, 	       -      
 ��$- 
       8-       ��L-        `-       ��t-        �-       ���-        �-       ���-        �-       ���-            � ߱        �  $  �"  �  ���                        	  $  �"  �  ���                        .                        � ߱        x	  $  �"  L	  ���                       .                        � ߱            �"  �	  
      .     4   ����.               �
                      ��                  �"  �"                  �;�                       �"  �	  `.      	   �. 	       �.      
   �. 
       �.         0/        D/         |/        �/         �/        �/         0            � ߱            $  �"   
  ���                       �    �"     �      (0     4   ����(0               d                      ��                  �"  �"                  X>�                       �"  0  �0      	   �0 	       �0      
   (1 
       <1         t1        �1         �1        �1         2         2         X2            � ߱            $  �"  �  ���                           �"  �  (      l2     4   ����l2               �                      ��                  �"  �"                  �@�                       �"  �  D3      	   |3 	       �3      
   �3 
       �3         4        (4         `4        t4         �4        �4         �4            � ߱            $  �"  8  ���                       �    �"  8  �      5     4   ����5               |                      ��                  �"  �"                  C�                       �"  H  �5      	   $6 	       86      
   p6 
       �6         �6        �6         7        7         T7        h7         �7            � ߱            $  �"  �  ���                       �    �"  �  @      �7     4   �����7               P                      ��                  �"  #                  pE�                       �"  �  ,    �"  l  |  �  �7     4   �����7     $  �"  �  ���                       �7                        � ߱            $   #     ���                       (8                        � ߱            $  #  X  ���                       �8                        � ߱        `    #  �        �8     4   �����8               ,                      ��                  #  #                  F�                       #  �      #  H  X  �  9     4   ����9     $  #  �  ���                       (9                        � ߱            $  #  �  ���                       49                        � ߱            $  #  4  ���                       T9                        � ߱          A  	#       �   ��        � �                                             �9                  �           �9          �9        �            �   �    H    
#  4  �      �9     4   �����9               �                      ��                  
#  #                  �G�                       
#  D  �  9   #         $  #  �  ���                       �9                        � ߱        �9          :        4:         �:        �:         �:        �:         L;        `;         �;        �;         <        (<      	   x< 	       �<         �<        �<         @=        T=         �=        �=         >        >         l>        �>         �>        �>      	   4? 	       H?         �?        �?         �?        @         `@        t@         �@        �@         (A        <A         �A        �A      	   �A 	       B         TB        hB         �B        �B         C        0C         �C        �C         �C        �C         HD        \D      	   �D 	           � ߱            $  #  (  ���                         ��                             ��                              ��        	                  ����                                =   +#              h   d d     �    ��H2�I2  � �                                               	                                                                         d     D                                                                 `  d d                                                         m        $                \  �+d X�                                  d                 �-      �         @      `  .d                                                           W        $                  \  .d X�             �                  O                 �-      �        H     
 X    <(d                                                        4     	      P   8�d                                                           �-  G   
 X  8xd                                             
           �     	  
    P   P8Xd                                                           �-  G   
 X  P8xd                                                        q     	  
    h  L�iM                                                        �     *	     �-               P   �d                                                           �-  G     p  �Xl                                                        �     	                            h  L�iM                                                        �     *	     �(              
 X  �xd                                                         �     1	     
 X  ��Ld 	                                            "           D     	      \  0*�d 
                                {          B       �-                @     
 X  �d                                             D                	      h  mtM                                             @           �     *	     �-               h  L�iM                                                        �     *	     �-               P   �-d                                                           �-  G     p  ��l                                             $           �     	                      �   h  ���M                                             <           �     *	      .               P   _�d                                                           .  G     p  _�l                                             &           �     	                      �   h  �_�M                                             >           �     *	     ).               P   *��d                                                           <.  G   
 X  *�xd                                             F           �     7	     
 X  �Ld                                             H           	     	      \  0*3�d                                 �          J       �-                @     
 X  7�d                                             L           %     	      P   ��d                                                           C.  G   
 X  �xd                                             (           �     1	     
 X  ��Ld                                             *           S     	      h  LDiM                                                        �     *	     8)              
 X  D�d                                             ,           �     <	     
 X  �Dl d                                             .           b     	      H   �P-�                                  x          N          H   {P-^                                           P          H   �P-P                                 �          R          H   )P-�                                  �          T          H    P-�                                  �          V           D                                                                    TXS appSrvUtils ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST s-codcia cl-codcia pv-codcia s-user-id s-aplic-id input-var-1 input-var-2 input-var-3 output-var-1 output-var-2 output-var-3 T-Vtamn T-Vtame T-Ctomn T-Ctome T-Promn T-Prome F-Salida X-FECHA cListaDivisiones tmp-detalle Llave CanxMes VtaxMesMe VtaxMesMn CtoxMesMe CtoxMesMn ProxMesMe ProxMesMn Detalle Campania Periodo NroMes Division CanalVenta Producto Linea Sublinea Marca Unidad Licencia Proveedor Cliente Canal Tarjeta Departamento Provincia Distrito Zona Clasificacion Vendedor CanActual CanAcumActual CanAnterior CanAcumAnterior VtaSolesActual VtaDolarActual CtoSolesActual CtoDolarActual PromSolesActual PromDolarActual VtaSolesAcumActual VtaDolarAcumActual CtoSolesAcumActual CtoDolarAcumActual PromSolesAcumActual PromDolarAcumActual VtaSolesAnterior VtaDolarAnterior CtoSolesAnterior CtoDolarAnterior PromSolesAnterior PromDolarAnterior VtaSolesAcumAnterior VtaDolarAcumAnterior CtoSolesAcumAnterior CtoDolarAcumAnterior PromSolesAcumAnterior PromDolarAcumAnterior T-Detalle x-CodDiv x-CodCli x-ClfCli x-CodMat x-CodPro x-CodVen x-CodFam x-SubFam x-CanalVenta x-Canal x-Giro x-NroCard x-Zona x-CodDept x-CodProv x-CodDist x-CuentaReg x-MuestraReg iContador x-NroFchR x-NroFchE x-Llave REPORTE pParametro +COSTO s-ConCostos s-TodasLasDivisiones s-Familia010 EstadTabla Tabla general de estadisticas EST -COSTO tt-cliente tt-codcli tt-nomcli tt-articulo tt-codmat tt-desmat tt-datos tt-codigo pOptions lOptions pArchivo cArchivo zArchivo cComando pDirectorio wWin BtnDone img/exit.ico BUTTON-1 img/excel.bmp BUTTON-5 BUTTON-6 COMBO-BOX-CodDiv COMBO-BOX-CodFam Todos COMBO-BOX-SubFam DesdeF FILL-IN-CodCli FILL-IN-CodMat FILL-IN-CodPro FILL-IN-CodVen FILL-IN-DesMat FILL-IN-file FILL-IN-file-2 FILL-IN-Mensaje FILL-IN-NomCli FILL-IN-NomPro FILL-IN-NomVen HastaF RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 TOGGLE-CodCli TOGGLE-CodDiv TOGGLE-CodMat TOGGLE-CodVen TOGGLE-Resumen-Depto TOGGLE-Resumen-Linea TOGGLE-Resumen-Marca fMain X(256) 99/99/9999 yes/no X(11) X(6) X(3) GUI ESTADISTICAS DE VENTAS COMPARATIVAS DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 BUTTON-1 BtnDone DesdeF HastaF TOGGLE-CodDiv TOGGLE-CodCli TOGGLE-CodMat TOGGLE-CodVen CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE Debe seleccionar por lo menos uno  GENERAL mdy dmy No hay datos que imprimir 9999 . .xls .zip .txt "C:\Archivos de programa\7-Zip\7z.exe" a    NO se pudo encriptar el archivo Avise a sistemas copy  Proceso terminado OKpressed Archivos Texto (*.txt) *.txt Archivo(s) de Carga... DimSubLinea Dimension Sub-Linea  -  DimCliente Dimensi�n Clientes Cliente no registrado Clientes DimProducto Dimensi�n Productos Articulo Inv�lido DimProveedor Dimensi�n Proveedores Proveedor no registrado Proveedores DimVendedor Dimensi�n Vendedores Vendedor no registrado VALUE-CHANGED iStartPage ADM-ERROR ADM-CREATE-OBJECTS , CARGA-LISTA-ARTICULOS x-linea CARGA-LISTA-CLIENTES Control-Resumen k xListaDivisiones EstadUserLinea Costos x lineas x usuarios FieldList: : Division,CanalVenta Cliente Canal,Departamento,Provincia,Distrito,Zona Producto,Linea,Sublinea,Marca,Unidad Linea,Sublinea Marca Licencia,Proveedor Vendedor ,CanActual,CanAcumActual,CanAnterior,CanAcumAnterior ,VtaSolesActual,VtaSolesAcumActual,VtaSolesAnterior,VtaSolesAcumAnterior CARGA-TEMPORAL DISABLE_UI ENABLE_UI i-Campo x-Campo x-Cuenta-Registros ** GENERANDO EL EXCEL ** | EXCEL-2 EXITOBJECT EstadUserDiv DimDivision Dimensi�n Divisiones DimLinea Dimensi�n Lineas y Sublineas INITIALIZEOBJECT Ventas DimFecha Dimesi�n Fecha ** TABLA GENERAL   FECHA  99  DIVISION   CLIENTE   ** DimTarjeta Dimensi�n Tarjeta Cliente Excl DimUbicacion Dimensi�n Ubicaci�n Geogr�fica DimLicencia dwh_Vendedor  Vendedor RESUMEN-GENERAL VentasxCliente ** TABLA POR CLIENTE  RESUMEN-POR-CLIENTE VentasxClienteLinea ** TABLA POR CLIENTE Y PRODUCTO dwh_Licencia Licencias RESUMEN-POR-CLIMAT VentasxVendedor ** TABLA POR DIVISION  RESUMEN-POR-DIVISION 010 999998 RESUMIDO FAMILIA 010 UNI RESUMEN-POR-FAMILIA VentasxProducto ** TABLA POR PRODUCTO   PRODUCTO  RESUMEN-POR-PRODUCTO VentasxLinea ** TABLA RESUMEN POR PRODUCTO   LINEA  RESUMEN-POR-RESMAT VentasxVendCliente ** TABLA POR VENDEDOR Y CLIENTE   VENDEDOR  RESUMEN-POR-VENDCLI ** TABLA POR VENDEDOR  RESUMEN-POR-VENDEDOR Llave01 default idx01 Button 1 &Done Desde Hasta Division ... Resumido por Departamento-Provincia Art�culo Linea Resumido por Linea y Sub-Linea Sub-linea Resumido por Marca Codigo Proveedor Idx00 Index01 Idx01 Index00 �'  �S  (  ([      ) �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled              �     cType       �     6   �          �                  getObjectType   �
  �
  �
  ,  !        
   hReposBuffer    L  !      @  
   hPropTable  h  !      `  
   hBuffer     !      |  
   hTable  �  �     7             �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �      "        
   hProc       "      <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �      H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                     T	  �	     =                                       �	  �	     >                                   '  (  )  �	  ,
     ?                                   :  <  �	  d
  .   @                                   F  N  X  \  a  b  c  d  s  t  v  w  x  y  z  {  }  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �      #      0     OKpressed   4
  l     A                                 �  �  �  �      $      �     OKpressed   <  �     B   |                              �  �  �  �  �    	   C                                   �  �  �  �  �  �  �  �  �  �  `  	   D                                   �  �  �               0  �     E                                         �  �  	   F                                              !  "  #  $  �  D  	   G                                   .  /  0  1  2  3  4  5  6    �     H                                   ?  @  B  h  �  	   I                                   L  M  N  O  P  Q  R  S  T  �  (     J                                   ^  _  `  a  c  f  g  �  t     K                                   q  r  D  �     L                                   |  }  ~    �  �  �  �  |  �     M                                   �  �  �  4     N                                   �  �  �  �  �  �  �  �    �     O                                   �  �  T  �     P                                   �  �  �  �  �  �     Q                                   �  �  �  H     R               4                  adm-create-objects      �  !   S               |                  Carga-Lista-Articulos       !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /  0  3  4  6  7  9  :  ;  <  =  ?  @  A  B  C  F      *      ,     x-linea L  |     T             d                  Carga-Lista-Clientes    U  `  a  b  c  d  e  f  g  h  i  k  l  m  o  p  q  r  s  u  v  x  y  �     +      �     Control-Resumen   +           k       ,     ,     xListaDivisiones    4  �  �   U   �          p                  Carga-Temporal  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                     	  
                                               "  #  %  (  +  ,  -  1  2  C  D  E  F  K  U  V  Y  Z  [  \  _  `  c  d  e  f  i  j  k  l  o  p  q  t  u  v  y  z  {  ~    �  �  �  �  �  @  <     V               0                  disable_UI  �  �  �  �     �     W               |                  enable_UI   �  �  �  �  �  .      �     i-Campo �  .      �     x-Campo     .      �     x-Cuenta-Registros  L  0  t   X   �          (                  Excel-2 �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                 	  
                                     !  #  $  %  '  (  )  +  ,  -  /  0  1  2  3  4  6  7  8  :  ;  <  =  >  @  A  B  C  D  F  G  H  J  K  L  M  N  P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _  `  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  �  �  �  <     Y               0                  exitObject  �  �  �     �     Z               x                  initializeObject    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  H    �   [                                 Resumen-General �  �  �  �  �  �            #  $  &  '  /  1  2  :  <  =  E  G  H  P  [  \  ]  ^  _  `  a  j  k  l  m  n  p  r  t  u  v  x  |  }  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    	  
    $  3  4  8  :  B  C  E  F  N  P  Q  Y  [  \  d  f  g  o  z  {  |  }  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  %  '  �  H  |   \               4                  Resumen-por-cliente 4  7  8  9  :  ;  T  \  ]  a  c  k  l  n  o  w  y  z  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  0  3  4  5  N  V  W  [  ]  e  f  h  i  q  s  t  |  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  *  ,    |  �   ]               h                  Resumen-por-climat  9  <  =  >  ?  @  Y  e  f  j  l  t  u  w  x  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    N  Q  R  S  l  x  y  }    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                 
            ,  a  d  8  D!  !   ^               ,!                  Resumen-por-division    t  y  z  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �      >      �!     x-Llave �   ("     _   �!          "                  Resumen-por-familia �  �  �      9  :  <  =  >  ?  A  �!  �"  ;   `               �"                  Resumen-por-producto    R  ]  ^  b  d  l  m  o  p  x  z  {  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  X"  �#  ;   a               �#                  Resumen-por-resmat  �        	                     !   "   *   ,   -   5   7   8   @   K   L   M   N   O   P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _   `   a   b   c   d   e   f   g   h   i   j   l   m   n   o   p   q   �   �   �#   %  �   b               �$                  Resumen-por-vendcli �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   !  !  !  !  !  !  !  !  !  !  !  !   !  "!  $!  &!  '!  (!  *!  .!  /!  7!  8!  :!  ;!  <!  =!  >!  ?!  @!  B!  C!  D!  E!  G!  H!  I!  J!  K!  L!  i!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  "  "  "  "  "  "  "  "  "  "  "  "  "  !"  #"  %"  &"  '"  )"  -"  ."  6"  7"  9"  :"  ;"  <"  ="  >"  ?"  A"  B"  C"  D"  F"  G"  H"  I"  J"  K"  h"  �"  �"  �$  P'  %   c               8'                  Resumen-por-vendedor    �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"   #  #  #  #  #  #  #  #  	#  
#  #  #  #  #  +#  -#  '  (F      . p4      �B                      �(  $(  0(     tmp-detalle �(         �(        �(        �(        �(        �(        �(        �(        Llave   CanxMes VtaxMesMe   VtaxMesMn   CtoxMesMe   CtoxMesMn   ProxMesMe   ProxMesMn   D.  �(   )  1   Detalle L+         X+         `+         h+         t+         �+         �+         �+         �+         �+         �+         �+         �+         �+         �+         �+         �+         �+         ,         ,          ,         ,,         8,         H,         T,         d,         t,         �,         �,         �,         �,         �,         �,         �,          -         -         (-         <-         P-         d-         x-         �-         �-         �-         �-         �-         �-         .         ,.         Campania    Periodo NroMes  Division    CanalVenta  Producto    Linea   Sublinea    Marca   Unidad  Licencia    Proveedor   Cliente Canal   Tarjeta Departamento    Provincia   Distrito    Zona    Clasificacion   Vendedor    CanActual   CanAcumActual   CanAnterior CanAcumAnterior VtaSolesActual  VtaDolarActual  CtoSolesActual  CtoDolarActual  PromSolesActual PromDolarActual VtaSolesAcumActual  VtaDolarAcumActual  CtoSolesAcumActual  CtoDolarAcumActual  PromSolesAcumActual PromDolarAcumActual VtaSolesAnterior    VtaDolarAnterior    CtoSolesAnterior    CtoDolarAnterior    PromSolesAnterior   PromDolarAnterior   VtaSolesAcumAnterior    VtaDolarAcumAnterior    CtoSolesAcumAnterior    CtoDolarAcumAnterior    PromSolesAcumAnterior   PromDolarAcumAnterior   �3  T.  `.  1   T-Detalle   �0         �0         �0         �0         �0         �0         �0         �0          1         1         1         1         (1         01         81         @1         P1         \1         h1         p1         �1         �1         �1         �1         �1         �1         �1         �1         �1         2         2         $2         82         L2         `2         t2         �2         �2         �2         �2         �2         �2          3         3         ,3         D3         \3         t3         �3         Campania    Periodo NroMes  Division    CanalVenta  Producto    Linea   Sublinea    Marca   Unidad  Licencia    Proveedor   Cliente Canal   Tarjeta Departamento    Provincia   Distrito    Zona    Clasificacion   Vendedor    CanActual   CanAcumActual   CanAnterior CanAcumAnterior VtaSolesActual  VtaDolarActual  CtoSolesActual  CtoDolarActual  PromSolesActual PromDolarActual VtaSolesAcumActual  VtaDolarAcumActual  CtoSolesAcumActual  CtoDolarAcumActual  PromSolesAcumActual PromDolarAcumActual VtaSolesAnterior    VtaDolarAnterior    CtoSolesAnterior    CtoDolarAnterior    PromSolesAnterior   PromDolarAnterior   VtaSolesAcumAnterior    VtaDolarAcumAnterior    CtoSolesAcumAnterior    CtoDolarAcumAnterior    PromSolesAcumAnterior   PromDolarAcumAnterior   �3  �3  �3     tt-cliente  �3         �3         tt-codcli   tt-nomcli   <4   4  4     tt-articulo $4         04         tt-codmat   tt-desmat       L4  X4     tt-datos    d4         tt-codigo   �4          �4  
   appSrvUtils �4        �4     s-codcia    �4        �4     cl-codcia   �4        �4     pv-codcia   5        5     s-user-id   05        $5     s-aplic-id  P5       D5     input-var-1 p5       d5     input-var-2 �5       �5     input-var-3 �5       �5     output-var-1    �5       �5     output-var-2    �5       �5     output-var-3    6    	  6     T-Vtamn 46    
  ,6     T-Vtame P6      H6     T-Ctomn l6      d6     T-Ctome �6      �6     T-Promn �6      �6     T-Prome �6      �6     F-Salida    �6       �6     X-FECHA 7       �6     cListaDivisiones    (7  
 
    7     x-CodDiv    H7  
 
    <7     x-CodCli    h7  
 
    \7     x-ClfCli    �7  
 
    |7     x-CodMat    �7  
 
    �7     x-CodPro    �7  
 
    �7     x-CodVen    �7  
 
    �7     x-CodFam    8  
 
 	   �7     x-SubFam    ,8  
 
 
   8     x-CanalVenta    H8  
 
    @8     x-Canal d8  
 
    \8     x-Giro  �8  
 
    x8     x-NroCard   �8  
 
    �8     x-Zona  �8  
 
    �8     x-CodDept   �8  
 
    �8     x-CodProv    9  
 
    �8     x-CodDist    9  
 
    9     x-CuentaReg D9  
 
    49     x-MuestraReg    d9  
 
    X9     iContador   �9  
 
    x9     x-NroFchR   �9  
 
    �9     x-NroFchE   �9       �9     x-Llave �9       �9     pParametro   :  
 
    �9     s-ConCostos ,:  
 
    :     s-TodasLasDivisiones    P:  
 
    @:     s-Familia010    p:       d:     pOptions    �:       �:     lOptions    �:       �:     pArchivo    �:       �:     cArchivo    �:       �:     zArchivo    ;       ;     cComando    0;       $;     pDirectorio L;  
 
    D;  
   wWin    t;  
 
    `;     COMBO-BOX-CodDiv    �;  
 
    �;     COMBO-BOX-CodFam    �;  
 
    �;     COMBO-BOX-SubFam    �;  
 
    �;     DesdeF  <  
 
    �;     FILL-IN-CodCli  (<  
 
     <     FILL-IN-CodMat  L<  
 
 !   <<     FILL-IN-CodPro  p<  
 
 "   `<     FILL-IN-CodVen  �<  
 
 #   �<     FILL-IN-DesMat  �<  
 
 $   �<     FILL-IN-file    �<  
 
 %   �<     FILL-IN-file-2   =  
 
 &   �<     FILL-IN-Mensaje $=  
 
 '   =     FILL-IN-NomCli  H=  
 
 (   8=     FILL-IN-NomPro  l=  
 
 )   \=     FILL-IN-NomVen  �=  
 
 *   �=     HastaF  �=  
 
 +   �=     TOGGLE-CodCli   �=  
 
 ,   �=     TOGGLE-CodDiv   �=  
 
 -   �=     TOGGLE-CodMat   >  
 
 .   >     TOGGLE-CodVen   D>  
 
 /   ,>     TOGGLE-Resumen-Depto    p>  
 
 0   X>     TOGGLE-Resumen-Linea    �>  
 
 1   �>     TOGGLE-Resumen-Marca    �>        �>  
   gshAstraAppserver   �>        �>  
   gshSessionManager   ?         ?  
   gshRIManager    8?        $?  
   gshSecurityManager  `?        L?  
   gshProfileManager   �?        t?  
   gshRepositoryManager    �?        �?  
   gshTranslationManager   �?        �?  
   gshWebManager    @        �?     gscSessionId    $@        @     gsdSessionObj   H@        8@  
   gshFinManager   l@        \@  
   gshGenManager   �@        �@  
   gshAgnManager   �@        �@     gsdTempUniqueID �@        �@     gsdUserObj  �@        �@     gsdRenderTypeObj    $A        A     gsdSessionScopeObj  @A  
 
 2   8A  
   ghProp  `A  
 
 3   TA  
   ghADMProps  �A  
 
 4   tA  
   ghADMPropsBuf   �A  
 
 5   �A     glADMLoadFromRepos  �A  
 
 6   �A     glADMOk �A  
 
 7   �A  
   ghContainer B  
 
 8   �A     cObjectName $B  
 
 9   B     iStart  DB  
 
 :   8B     cAppService dB  
 
 ;   XB     cASDivision �B  
 
 <   xB     cServerOperatingMode    �B  
 
 =   �B     cFields     
 
 >   �B     iStartPage  �B    L  �B  tmp-detalle  C    L  �B  Detalle C  	 	 L  C  T-Detalle   8C       ,C  EstadTabla  TC    L  HC  tt-cliente  pC    L  dC  tt-articulo �C    L  �C  tt-datos    �C    %    �C  DimSubLinea �C  ! &    �C  DimCliente  �C  " '    �C  DimProducto  D  # (    �C  DimProveedor    D  $ )    D  DimVendedor <D  % -    ,D  EstadUserLinea  \D  & /    LD  EstadUserDiv    xD  ' 0    lD  DimDivision �D  ( 1    �D  DimLinea    �D  * 4    �D  Ventas  �D  + 5    �D  DimFecha    �D  , 6    �D  DimTarjeta  E  - 7    �D  DimUbicacion     E  . 8    E  DimLicencia @E  / 9    0E  dwh_Vendedor    `E  0 :    PE  VentasxCliente  �E  1 ;    pE  VentasxClienteLinea �E  2 <    �E  dwh_Licencia    �E  3 =    �E  VentasxVendedor �E  4 ?    �E  VentasxProducto F  5 @    �E  VentasxLinea        6 A    F  VentasxVendCliente           7   m  p  q  u  v  �  �  �  �  �  �  �  �  �  �  O	  P	  Q	  R	  i	  u	  v	  w	  y	  {	  |	  }	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  F
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
  B  M  N  P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _  `  a  b  c  d  e  f  g  h  i  j  k  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                   	  
                                             !  "  #  $  %  &  '  (  )  �  �  �  �  �  �  �  �  �  �  �  �    -  R  n  p  �    %  &  @  P  Q  R  U  V  W  ^  _  |  �  �  E  F  J  T  �  �  �  �  �  �  �  �  �  �  �    
                �  �    	    #  1  D  �  �  �  �      ,  =  J  \  o  z  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �      �� ( .\APLIC\est\resumenxvendcli-com.i    �J  B� ' .\APLIC\est\resumenxclimat-com.i �J  �= & .\APLIC\est\resumenxcliente-com.i    �J  �� % .\APLIC\est\resumengeneral-com.i K  H� $ C:\Progress\OpenEdge\src\adm2\windowmn.i DK  f!  C:\Progress\OpenEdge\src\adm2\containr.i xK  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �K  ��  C:\Progress\OpenEdge\src\adm2\visual.i   �K  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  $L  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    dL  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �L  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �L  Ds   C:\Progress\OpenEdge\gui\fn  M  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   <M  Q.  C:\Progress\OpenEdge\gui\set |M  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �M  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �M  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    N  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  `N  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i �N  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �N  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i O  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    HO  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �O  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �O  �j  C:\Progress\OpenEdge\gui\get P  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    ,P  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    pP  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �P  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �P  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i Q  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   \Q  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �Q  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �Q  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  R  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  PR  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i �R  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �R  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   S  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   HS  P�    C:\newsie\on_in_co\aplic\Est\w-estad-003.w       Z	  �"      �S     �!  (   �S  S	  �!      �S     �   (   �S    a      �S     U  '   T    N      T     B  '   (T  �  *      8T     7  &   HT  �  0      XT     =  &   hT  �  %      xT       %   �T  �        �T     �  %   �T  �        �T     �  $   �T  �  �      �T  �   �     �T     �     �T  �   �     U     �     U  �   |     (U     #  #   8U  �        HU           XU  �        hU           xU  �        �U     �      �U  r   �     �U  n   �     �U     s  "   �U  i   n     �U     L     �U  P   3     �U  �   *     V     �  !   V  �   �     (V     �     8V  �   �     HV     �     XV  �   �     hV     d     xV  g   J     �V     +     �V  O        �V  �   �     �V     �      �V  �   k     �V          �V  �        �V     �     W  �   �     W     �     (W  �   �     8W     �     HW  �   �     XW     }     hW  �   l     xW     J     �W  �   G     �W     %     �W  }        �W     �     �W     {     �W     -     �W     �     �W  7   �     X  �   �     X  O   �     (X     {     8X     -     HX  �   �     XX  �   �     hX  O   �     xX     �     �X     o     �X  �   J     �X  x   B  
   �X  M   -     �X          �X     �
     �X  a   �
  
   �X  �  �
     Y     y
     Y  �  F
     (Y  O   8
     8Y     '
     HY     �	     XY  �   	     hY     �     xY     *     �Y  x   $     �Y          �Y     �     �Y     �     �Y     |     �Y     c     �Y  Q   S  
   �Y     �     Z     �  
   Z     �     (Z     �  
   8Z  f   h     HZ       	   XZ  "   �     hZ     �     xZ     �     �Z  Z   =     �Z     E     �Z          �Z     �     �Z     �     �Z     �     �Z  '   �       �Z     @      [            [           