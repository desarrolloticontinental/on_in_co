	��V57�a�4  6�              K                                �� 34F8010Dutf-8 MAIN d:\newsie\on_in_co\APLIC\ccb\wenvletbco-01.w,, PROCEDURE Regresar,, PROCEDURE initializeObject,, PROCEDURE exitObject,, PROCEDURE Enviar,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE Carga-Temporales,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER      0�              �             �a 0�  ��              �j              @  
  +   t� �  7   � `  8   t� �   G   h� �	  H    |  I   �    J   � �
  K   � $  L   � �  M   � �
  N           �* �  �. �  �2 4  ? �: �&  iSO8859-1                                                                           Ƞ   ! �                                      �                  ��   
             h�  �#    $   �.   x�  ��         h�  �   ��      �          L                                             PROGRESS                         �           
    
                    �              �                                                                                                     
           �          L  ^  �   �^     �  �a�a�a  ~                     @&          ,0      �   l         �         L  P�  �   �    �  �a�aО  ~                    �c          pm      �   �             �                                                                                          �             d             P                                                                                          �                          �                                                                                          �             �          
    
                  �  �             x                                                                                                    
  8  #      �  
    
                  �  h  	           $                                                                                          #          
  �  5      `  
    
                  L    
           �                                                                                          5          
  �  B        
    
                  �  �             |                                                                                          B          
  <  U      �  
    
                  �  l             (                                                                                          U          
  �  g      d  
    
                  P               �                                                                                          g          
  �  |        
    
                  �  �             �                                                                                          |          
  @	  �      �  
    
                  �  p	             ,	                                                                                          �          
  �	  �      h	                         T	  
             �	                                                                                          �            �
  �      
                         
  �
             �
                                                                                          �            D  �      �
  
    
                  �
  t             0                                                                                          �          
  �  �      l  
    
                  X                �                                                                                          �          
  �  �        
    
                    �             �                                                                                          �          
  H  �      �                        �  x             4                                                                                          �            �  �      p                        \  $             �                                                                                          �            �                                   �             �                                                                                                                 �                        �  �             8                                                                                                                   INTEGRAL                         PROGRESS                         �     �$  L      �$                         �ɺ[            �$  �4                              �                        L  ,        CODCIANOMAGECODAGEDIRAGECODBCO                                                         �$  L      �$                         �ɺ[            �$  Í                              �                        (    (     CODCTANOMCTAAFTDCBPIDAUXPIDDOCCIERESCODMONTPOCMBACTIVOCLFAUXPIDREFNROCHQCODDOCCODBCONROCTAAN1CTACC1CTASECTORCODDIVTPOGTOCODCIAPIDCCOVCODCIACHQFINCHQINICTAANTCODOPETMMOVCTASUSTENTOLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_F03LIBRE_L01LIBRE_L02                                                                         	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )                P%  L      P%                         �a�a            P%  �                              �  �                        �  ~~     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2CODCAJADCTO_OTROS_MOTDCTO_OTROS_FACTORDCTO_OTROS_VVDCTO_OTROS_PVLISTA_DE_PRECIOSTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                            �%  L      �%                         �M�]            �%  ~                              �  �                      t!  �  �3     CODDIVDESDIVCODCIADIRDIVTELDIVFAXDIVRESPONFLGREPFLGPREUNIFLGAPRCOTFLGAPRPEDFLGPREVTADIASVTOCOTDIASVTOPEDDIASVTOO_DDIASAMPCOTTIPDTOMODPREUNIFLGEMPAQUEFLGROTACIONCANALVENTAFLGPICKINGFLGBARRASFLGMINVENTAFLGDTOPROMFLGDTOVOLFLGTARJETAVENTAMOSTRADORVENTAMAYORISTAVENTAMINORISTAFLGDTOCLFCLIPORDTOCLFCLIFLGDTOCNDVTALIBRE_C01LIBRE_C02LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02FLAGTIPOFLAGUSUARIOFLAGMIGRACIONFLAGFECHACAMPO-LOGCAMPO-DECCAMPO-DATECAMPO-CHARGRUPO_DIVI_GGCENTRO_COSTO                                                                       	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          .          /          0 
        1 
        2 
        3 
        4          5                        �                                               �          �%  �%  X �@$                                        
                                                                                   
             
             
                                         
                                                                                                                X   h   x   �   �   �   �   �   �   �   �       (  8  H  X  h  x  �  �      X   h   x   �   �   �   �   �   �   �   �      (  8  H  X  h  x  �  �                                                                                                                                     	                  
                                                                                                                                                                                                                                          9                                                                                                                                                                   !                  "                  #                  $                  I                  %                  &                  '                  (                  )                                                      *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  ;                  <                  :                  =                  >                  ?                  @                  A                  B                  C                  D                  E                  F                  G                  H                  J                  K              
   L                  M              
   N                  O                  P                  Q                  R                  S                  T                  U                  V                  W                  X                  Y                  Z                  [                  \                  ]                  ^                  _                  `                  a                  b                  c                  d                  e                  f                  g                  h                  i                  j                  k                  l                  m                  n                  o                  p                  q                  r                  s                  t                  u                  v                  w                  x                  y                  z                  {                  |                  }                  ~                                                    F  F  F  F  F          F             ,F  4F  <F  LF  DF                         PF  XF  `F  pF  hF                         tF  |F  �F  �F  �F                         �F  �F  �F  �F  �F                         �F  �F  �F  �F  �F                         �F  �F  �F  G  G                          G  G  $G  ,G  (G                          0G  8G  @G  `G  PG                          dG  lG  tG  |G                             �G  �G  �G  �G  �G                         �G  �G  �G  �G  �G                          �G  �G  �G  H   H                         H  H  ,H  TH  @H                         XH  `H  hH  �H  tH                         �H  �H  �H  �H  �H                         �H  �H  �H   I  �H                         I  I  I  <I  ,I                         @I  HI  XI  xI  hI                         |I  �I  �I  �I  �I                         �I  �I  �I  �I  �I                          �I  �I  �I  �I  �I                          �I   J  J  J                              J  J  $J  4J  ,J                          8J  @J  HJ  hJ  XJ                          lJ  tJ  xJ  �J  �J                          �J  �J  �J  �J  �J                         �J  �J  �J  �J  �J                         �J  �J  �J  K  K                           K  (K  0K  XK  DK                          \K  dK  hK  �K  �K          �K              �K  �K  �K  �K  �K                          �K  �K  �K  L  �K                         L  L  $L  <L  0L                         @L  HL  XL  xL  hL                         |L  �L  �L  �L  �L                          �L  �L  �L  �L  �L                          �L   M  M  M  M                         M  $M  ,M  <M  4M                         @M  HM  TM  �M  lM                         �M  �M  �M  �M  �M                          �M  �M  �M  �M  �M                         �M  �M  �M   N  �M                          N  N  N  (N  N                          ,N  4N  8N  PN  DN                          TN  \N  hN  �N  |N                          �N  �N  �N  �N  �N                          �N  �N  �N  �N  �N                          �N  �N  �N  O  O                          O   O  (O  8O  0O                         @O  HO  XO  pO  dO                         tO  |O  �O  �O  �O                          �O  �O  �O  �O                              �O  �O  �O  �O                              �O  �O  P  P  P                           P  (P  0P  HP  <P                         LP  TP  \P  lP  dP                         pP  xP  �P  �P  �P                          �P  �P  �P  �P  �P                          �P  �P  �P  �P                              �P  �P  �P  Q   Q          Q              (Q  0Q  4Q  <Q  8Q          @Q              \Q  dQ  lQ  �Q  tQ          �Q              �Q  �Q  �Q  �Q  �Q                          �Q  �Q  �Q  �Q  �Q                          �Q  �Q  R  $R  R                          (R  0R  8R  PR  DR                          TR  \R  dR  |R  pR                          �R  �R  �R  �R  �R                         �R  �R  �R  �R  �R                          �R  �R  �R  S  S                         S  S  $S  4S  ,S          8S              XS  `S  pS  xS                             |S  �S  �S  �S  �S                         �S  �S  �S  �S  �S                         �S  �S  �S  �S  �S                          �S   T  T  T  T                          T  T  $T  DT  4T          HT              XT  `T  hT  �T                              �T  �T  �T  �T                             �T  �T  �T  �T                             �T  �T  �T  �T                              �T  �T  �T  U                              U  U  U  (U                              ,U  8U  @U  LU                              PU  \U  dU  pU                              tU  �U  �U  �U                              �U  �U  �U  �U                             �U  �U  �U  �U                             �U  V  V  V                               V  ,V  8V  DV                              HV  PV  \V  dV                              hV  tV  �V  �V                              �V  �V  �V  �V                              �V  �V  �V  �V                              �V  �V  �V  W                              W  W   W  4W                              8W  @W  HW  XW  PW                         \W  dW  tW  |W                             �W  �W  �W  �W                             �W  �W  �W  �W                              �W  �W  �W  �W  �W                          �W  X  X  $X                              (X  <X  LX  `X                             dX  tX  �X  �X                             �X  �X  �X  �X                             �X  �X  �X  Y                              Y  (Y  8Y  XY                             \Y  |Y  �Y  �Y                             �Y  �Y  �Y  �Y                             �Y  Z  Z  $Z                             (Z  8Z  HZ  XZ                             \Z  lZ  |Z  �Z                             �Z  �Z  �Z  �Z                             �Z  �Z  �Z  [                             [   [  ,[  D[                             H[  d[  t[  �[                             �[  �[  �[  �[                             �[   \  \  (\                             ,\  L\  \\  |\                             �\  �\  �\  �\                             �\  �\  �\  ]                             ]  ,]  <]  L]                             P]  d]  t]  �]                             �]  �]  �]  �]                             �]  �]  �]  ^                                                                         CodCia  999 Cia Cia 0   C�digo de compa�ia  CodDoc  x(3)    Codigo  Codigo      NroDoc  X(12)   Numero  Numero      FchDoc  99/99/9999  Fecha   Fecha   TODAY   CodCli  x(11)   Cliente Cliente     NomCli  x(50)   Nombre  Nombre      DirCli  x(60)   Direccion   Direccion       RucCli  x(11)   Ruc Ruc     CodAnt  X(10)   Codigo Anterior Codigo!Anterior     CodPed  x(10)   CodPed      NroPed  X(12)   Pedido  Pedido      NroOrd  x(12)   Orden de Compra Orden de!Compra     ImpBrt  ->>,>>>,>>9.99  Importe Bruto   Importe Bruto   0   ImpExo  ->>,>>>,>>9.99  Importe Exonerado   Importe Exonerado   0   PorIgv  ->>9.99 % I.G.V.    % I.G.V.    0   ImpIgv  ->>,>>>,>>9.99  Importe I.G.V.  Importe I.G.V.  0   ImpDto  ->>,>>>,>>9.99  Importe Descuento   Importe Descuento   0   ImpTot  ->>,>>>,>>9.99  Importe Total   Importe Total   0   SdoAct  ->>,>>>,>>9.99  Importe Total   Importe Total   0   FlgEst  X   Estado  Estado  P   usuario x(10)   usuario usuario     UsrDscto    X(10)   Resp.!Dscto.    Resp.!Dscto.        FlgCie  x   FlgCie  P   FchCie  99/99/9999  Cierre  Cierre  ?   HorCie  x(5)    Hora de cierre  Hora de!cierre      CodMon  9   Moneda  Moneda  1   TpoCmb  Z,ZZ9.9999  Tipo de cambio  T/Cambio    0   CodAlm  x(3)    Almacen Almacen     LugEnt  x(60)   Lugar de entrega    Lugar de entrega        Tipo    x(20)   Tipo de documento   Tipo de documento       CodMov  99  C�digo de movimiento    C�digo!movimto. 0   C�digo de movimiento    CodVen  x(10)   Vendedor    Vendedor        ImpIsc  ->>,>>>,>>9.99  Importe Isc Importe Isc 0   ImpVta  ->>,>>>,>>9.99  Valor Venta Valor venta 0   ImpFle  ->>,>>>,>>9.99  Importe Flete   Importe Flete   0   FchCan  99/99/9999  Fecha de cancelacion    Fecha de!cancelacion    ?   Glosa   x(60)   Observaciones   Observaciones       CodRef  x(3)    Codigo  Codigo      NroRef  X(12)   Numero  Numero      FchVto  99/99/9999  Fecha de vencimiento    Fecha de!Vencimiento    ?   CodCob  X(10)   Cobrador    Cobrador        CodCta  X(10)   Cuenta Contable Cuenta      CodAge  X(10)   Agencia Agencia     FlgUbi  X   Ubicaci�n   Ubicaci�n       FlgUbiA X   Ubicaci�n   Ubicaci�n       FchUbi  99/99/9999  Fecha de Ubicaci�n  Fecha   ?   FchUbiA 99/99/9999  Fecha de Ubicaci�n  Fecha   ?   FlgSit  X   Situaci�n   Situaci�n       Cndcre  X   Condicion de Credito    Condicion       CodDiv  x(5)    C.Div   C.Div   00000   ImpInt  ->>,>>>,>>9.99  Intereses   Intereses   0   FmaPgo  X(8)    Condicion de ventas Condicion de!venta      FchAct  99/99/9999  FchAct  ?   FlgSitA X   Situacion Anterior      TipVta  X(1)    Tipo Venta  Tipo venta      PorDto  >>9.99  % Dscto.    % Dscto.    0   TpoFac  X(1)    Tipo    Tipo        FchCbd  99/99/9999  Fecha   Fecha   ?   NroSal  X(12)   Numero Salida   Numero!Salida       FlgCbd  yes/no  FlgCbd  no  Codope  xxx Operacion   Ope     Ingrese la Operacion Contable   NroMes  99  Mes Mes 0   Ingrese el mes de trabajo   Nroast  x(6)    Asiento Comprobte       Ingrese el Nro. de Asiento  FchAnu  99/99/99    Fecha   Fecha!Anulacion ?   UsuAnu  X(10)   Usuario Usuario     CodDpto X(2)    Departamento    Departamento        CodProv X(2)    Provincia   Provincia       CodDist X(2)    Distrito    Distrito        FlgCon  x(1)    Flag Control    Flag!Control        LugEnt2 x(60)   Lugar de entrega    Lugar de entrega        FlgAte  X   Estado  Estado      FchAte  99/99/9999  Fecha   Fecha   ?   Fecha de Despacho de Almacen    imptot2 ->>>,>>>,>>9.99 imptot2 0   ImpCto  ->>,>>>,>>9.99  ImpCto  ImpCto  0   AcuBon  ->>>,>>>,>>9.99 AcuBon  AcuBon  0   NroCard x(8)    NroCard Nrocard     TipBon  99  TipBon  TipBon  0   CCo X(5)    Centro de Costo Centro!de Costo     Centro de Costo FlgEnv  Si/No   El pedido es para enviar?   No  puntos  ->>,>>9.99  puntos  0   mrguti  ->>,>>9.99  mrguti  0   Sede    x(5)    Sede        Libre_c01   x(60)   Libre_c01       Libre_c02   x(60)   Libre_c02       Libre_c03   x(60)   Libre_c03       Libre_c04   x(60)   Libre_c04       Libre_c05   x(60)   Libre_c05       Libre_d01   ->>>,>>>,>>9.99<<<  Libre_d01   0   Libre_d02   ->>>,>>>,>>9.99<<<  Libre_d02   0   Libre_f01   99/99/99    Libre_f01   ?   Libre_f02   99/99/99    Libre_f02   ?   FchCre  99/99/99    FchCre  ?   Libre_f03   99/99/99    Libre_f03   ?   Libre_f04   99/99/99    Libre_f04   ?   Libre_f05   99/99/99    Libre_f05   ?   FchCobranza 99/99/9999  Fecha Cobranza  ?   UsrCobranza x(10)   Usuario Cobranza        DivOri  x(5)    Origen  Origen      ImpPro  ->>>,>>>,>>9.99 ImpPro  0   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   GlosaImpDto2    x(30)   GlosaImpDto2        CodCaja X(10)   Codigo Caja Codigo!Caja     Dcto_Otros_Mot  x(20)   Dcto_Otros_Mot      Dcto_Otros_Factor   ->>,>>9.999999  Dcto_Otros_Factor   0   Dcto_Otros_VV   ->>>,>>>,>>9.999999 Dcto_Otros_VV   0   Dcto_Otros_PV   ->>>,>>>,>>9.999999 Dcto_Otros_PV   0   Lista_de_Precios    x(8)    Lista_de_Precios        TotalValorVentaNetoOpGravadas   >>>>>>>>>>>9.99 TotalValorVentaNetoOpGravadas   0   TotalValorVentaNetoOpGratuitas  >>>>>>>>>>>9.99 TotalValorVentaNetoOpGratuitas  0   TotalTributosOpeGratuitas   >>>>>>>>>>>9.99 TotalTributosOpeGratuitas   0   TotalIGV    >>>>>>>>>>>9.99 TotalIGV    0   TotalImpuestos  >>>>>>>>>>>9.99 TotalImpuestos  0   TotalValorVenta >>>>>>>>>>>9.99 TotalValorVenta 0   TotalPrecioVenta    >>>>>>>>>>>9.99 TotalPrecioVenta    0   DescuentosGlobales  >>>>>>>>>>>9.99 DescuentosGlobales  0   PorcentajeDsctoGlobal   >>9.99999   PorcentajeDsctoGlobal   0   MontoBaseDescuentoGlobal    >>>>>>>>>>>9.99 MontoBaseDescuentoGlobal    0   TotalValorVentaNetoOpNoGravada  >>>>>>>>>>>9.99 TotalValorVentaNetoOpNoGravada  0   TotalDocumentoAnticipo  >>>>>>>>>>>9.99 TotalDocumentoAnticipo  0   MontoBaseDsctoGlobalAnticipo    >>>>>>>>>>>9.99 MontoBaseDsctoGlobalAnticipo    0   PorcentajeDsctoGlobalAnticipo   >>9.99999   PorcentajeDsctoGlobalAnticipo   0   TotalDsctoGlobalesAnticipo  >>>>>>>>>>>9.99 TotalDsctoGlobalesAnticipo  0   MontoBaseICBPER >>>>>>>>>>>9.99 MontoBaseICBPER 0   TotalMontoICBPER    >>>>>>>>>>>9.99 TotalMontoICBPER    0   TotalValorVentaNetoOpExoneradas >>>>>>>>>>>9.99 TotalValorVentaNetoOpExoneradas 0   TotalVenta  >>>>>>>>>>>9.99 TotalVenta  0   �   2 B W � � ��  ���������   �               P   P�         �   �   ��  00000  �      �    �       �   �           � �           �            �������                                    �%        �%        �%        �%        �%        �%        �%        &        &        &        &        '&        /&        7&        ?&        G&        O&                �     i  i  i  i      i  i  i      i  i  i  i  i  i      i  i  i  i      i  i  i  i  i      i  i  i  i  i  j      i  i  i  i  i      i 	 i 
 i  i  i      i  i  i  i  i      i  i  i  i  i  i      i  i  i  i  i      i  i  i  i  i  i      i  i  i      i  i  i  i  i      i  i  i  i  i  i      i  i  i  i      i  i  i  i  i     	1 	 	 	 	) 	8 	 	 	' 	( 	 	E 	 	G 	 	 	 	c 	       "   )   0   7   >   E   L   S   Z   a   h   o   v   }   �   �   �   �   �   3  :  �   �   �   �   �   �   �   �   �   �   �   �           %  ,  A  H  O  W  ^  f  m  t  {  �  �  �  �  �  �  �   �  �  �  �  �  �  �  �  �  �  �  �        
    "  )  0  8  ?  C  J  Q  X  ]  g  q  {  �  �  �  �  �  �  �  �  �  �  �  �  �    
      .  @  N  \  m  �  �  �  �  �  �  �    &  ?  ^  u  �  �  �  �  �                                                                                                                                       	                  
                                                                                                                                                                                                                                          9                                                                                                                                                                   !                  "                  #                  $                  I                  %                  &                  '                  (                  )                                                      *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  ;                  <                  :                  =                  >                  ?                  @                  A                  B                  C                  D                  E                  F                  G                  H                  J                  K              
   L                  M              
   N                  O                  P                  Q                  R                  S                  T                  U                  V                  W                  X                  Y                  Z                  [                  \                  ]                  ^                  _                  `                  a                  b                  c                  d                  e                  f                  g                  h                  i                  j                  k                  l                  m                  n                  o                  p                  q                  r                  s                  t                  u                  v                  w                  x                  y                  z                  {                  |                  }                  ~                                                   D�  L�  P�  X�  T�          \�             p�  x�  ��  ��  ��                         ��  ��  ��  ��  ��                         ��  ��  ̃  ܃  ԃ                         �  �  �  �  ��                         �  �  �  (�   �                         ,�  4�  <�  T�  H�                          X�  `�  h�  p�  l�                          t�  |�  ��  ��  ��                          ��  ��  ��  ��                             Ą  ̄  Ԅ  �  ܄                         �  ��  ��  �  �                          �  $�  4�  T�  D�                         X�  `�  p�  ��  ��                         ��  ��  ��  ą  ��                         ȅ  Ѕ  ��   �  ��                         �  �  �  D�  0�                         H�  P�  `�  ��  p�                         ��  ��  ��  ��  ��                         ��  Ȇ  ̆  ܆  Ԇ                         ��  �  ��   �  ��                          �  �  �  8�  (�                          <�  D�  H�  P�                              T�  \�  h�  x�  p�                          |�  ��  ��  ��  ��                          ��  ��  ��  ̇  ć                          Ї  ؇  �   �  �                         �  �  �  $�  �                         (�  0�  8�  `�  L�                          d�  l�  t�  ��  ��                          ��  ��  ��  Ԉ  Ĉ          ؈              ��  ��   �  �  �                          �  $�  4�  L�  @�                         P�  X�  h�  ��  t�                         ��  ��  ��  ��  ��                         ��  ȉ  ԉ  �  �                          �  �  �  8�  (�                          <�  D�  L�  \�  T�                         `�  h�  p�  ��  x�                         ��  ��  ��  Ȋ  ��                         ̊  Ԋ  ܊  �  �                          ��   �  �   �  �                         $�  ,�  4�  D�  <�                          H�  P�  T�  l�  `�                          p�  x�  |�  ��  ��                          ��  ��  ��  ȋ  ��                          ̋  ԋ  ��  ��  �                           �  �  �  $�  �                          (�  0�  4�  X�  L�                          \�  d�  l�  |�  t�                         ��  ��  ��  ��  ��                         ��  ��  Ȍ  ��  ܌                          �  ��  �  �                              �  �   �  4�                              8�  @�  H�  `�  T�                          d�  l�  t�  ��  ��                         ��  ��  ��  ��  ��                         ��  ��  ȍ  ؍  Ѝ                          ܍  �  �  �  ��                          �  �   �  (�                              ,�  4�  8�  H�  D�          L�              l�  t�  x�  ��  |�          ��              ��  ��  ��  Ď  ��          Ȏ              �  �  ��  �   �                          �  �  $�  4�  ,�                          8�  @�  H�  h�  X�                          l�  t�  |�  ��  ��                          ��  ��  ��  ��  ��                          ď  ̏  ԏ  �  �                         ��   �  �  0�  �                          4�  <�  @�  P�  H�                         T�  \�  h�  x�  p�          |�              ��  ��  ��  ��                             ��  Ȑ  ؐ  �  ��                         �  ��  �  �  �                         �   �  (�  8�  0�                          <�  D�  H�  X�  P�                          \�  `�  h�  ��  x�          ��              ��  ��  ��  ȑ                              ̑  ԑ  ��  �                             �  ��   �  �                             �  �  �  $�                              (�  4�  <�  H�                              L�  X�  `�  l�                              p�  |�  ��  ��                              ��  ��  ��  ��                              ��  Ē  ̒  ؒ                              ܒ  �  ��  �                             �  �  ,�  8�                             <�  H�  T�  `�                              d�  p�  |�  ��                              ��  ��  ��  ��                              ��  ��  ē  Г                              ԓ  ��  �  ��                              ��  �  �   �                              $�  0�  <�  L�                              P�  \�  d�  x�                              |�  ��  ��  ��  ��                         ��  ��  ��  ��                             Ĕ  ̔  ܔ  �                             �  ��   �  �                              �  �  $�  <�  0�                          @�  P�  X�  h�                              l�  ��  ��  ��                             ��  ��  ̕  ܕ                             ��  �  �  �                             �  ,�  4�  H�                              L�  l�  |�  ��                             ��  ��  Ж  �                             ��  �   �  <�                             @�  L�  \�  h�                             l�  |�  ��  ��                             ��  ��  ��  З                             ԗ  �  ��  �                             �  $�  4�  H�                             L�  d�  p�  ��                             ��  ��  ��  Ԙ                             ؘ  ��  �  (�                             ,�  D�  T�  l�                             p�  ��  ��  ��                             ę  �  �  �                             �  0�  @�  \�                             `�  p�  ��  ��                             ��  ��  ��  ̚                             К  �   �   �                             $�  0�  @�  L�                                                                         CodCia  999 Cia Cia 0   C�digo de compa�ia  CodDoc  x(3)    Codigo  Codigo      NroDoc  X(12)   Numero  Numero      FchDoc  99/99/9999  Fecha   Fecha   TODAY   CodCli  x(11)   Cliente Cliente     NomCli  x(50)   Nombre  Nombre      DirCli  x(60)   Direccion   Direccion       RucCli  x(11)   Ruc Ruc     CodAnt  X(10)   Codigo Anterior Codigo!Anterior     CodPed  x(10)   CodPed      NroPed  X(12)   Pedido  Pedido      NroOrd  x(12)   Orden de Compra Orden de!Compra     ImpBrt  ->>,>>>,>>9.99  Importe Bruto   Importe Bruto   0   ImpExo  ->>,>>>,>>9.99  Importe Exonerado   Importe Exonerado   0   PorIgv  ->>9.99 % I.G.V.    % I.G.V.    0   ImpIgv  ->>,>>>,>>9.99  Importe I.G.V.  Importe I.G.V.  0   ImpDto  ->>,>>>,>>9.99  Importe Descuento   Importe Descuento   0   ImpTot  ->>,>>>,>>9.99  Importe Total   Importe Total   0   SdoAct  ->>,>>>,>>9.99  Importe Total   Importe Total   0   FlgEst  X   Estado  Estado  P   usuario x(10)   usuario usuario     UsrDscto    X(10)   Resp.!Dscto.    Resp.!Dscto.        FlgCie  x   FlgCie  P   FchCie  99/99/9999  Cierre  Cierre  ?   HorCie  x(5)    Hora de cierre  Hora de!cierre      CodMon  9   Moneda  Moneda  1   TpoCmb  Z,ZZ9.9999  Tipo de cambio  T/Cambio    0   CodAlm  x(3)    Almacen Almacen     LugEnt  x(60)   Lugar de entrega    Lugar de entrega        Tipo    x(20)   Tipo de documento   Tipo de documento       CodMov  99  C�digo de movimiento    C�digo!movimto. 0   C�digo de movimiento    CodVen  x(10)   Vendedor    Vendedor        ImpIsc  ->>,>>>,>>9.99  Importe Isc Importe Isc 0   ImpVta  ->>,>>>,>>9.99  Valor Venta Valor venta 0   ImpFle  ->>,>>>,>>9.99  Importe Flete   Importe Flete   0   FchCan  99/99/9999  Fecha de cancelacion    Fecha de!cancelacion    ?   Glosa   x(60)   Observaciones   Observaciones       CodRef  x(3)    Codigo  Codigo      NroRef  X(12)   Numero  Numero      FchVto  99/99/9999  Fecha de vencimiento    Fecha de!Vencimiento    ?   CodCob  X(10)   Cobrador    Cobrador        CodCta  X(10)   Cuenta Contable Cuenta      CodAge  X(10)   Agencia Agencia     FlgUbi  X   Ubicaci�n   Ubicaci�n       FlgUbiA X   Ubicaci�n   Ubicaci�n       FchUbi  99/99/9999  Fecha de Ubicaci�n  Fecha   ?   FchUbiA 99/99/9999  Fecha de Ubicaci�n  Fecha   ?   FlgSit  X   Situaci�n   Situaci�n       Cndcre  X   Condicion de Credito    Condicion       CodDiv  x(5)    C.Div   C.Div   00000   ImpInt  ->>,>>>,>>9.99  Intereses   Intereses   0   FmaPgo  X(8)    Condicion de ventas Condicion de!venta      FchAct  99/99/9999  FchAct  ?   FlgSitA X   Situacion Anterior      TipVta  X(1)    Tipo Venta  Tipo venta      PorDto  >>9.99  % Dscto.    % Dscto.    0   TpoFac  X(1)    Tipo    Tipo        FchCbd  99/99/9999  Fecha   Fecha   ?   NroSal  X(12)   Numero Salida   Numero!Salida       FlgCbd  yes/no  FlgCbd  no  Codope  xxx Operacion   Ope     Ingrese la Operacion Contable   NroMes  99  Mes Mes 0   Ingrese el mes de trabajo   Nroast  x(6)    Asiento Comprobte       Ingrese el Nro. de Asiento  FchAnu  99/99/99    Fecha   Fecha!Anulacion ?   UsuAnu  X(10)   Usuario Usuario     CodDpto X(2)    Departamento    Departamento        CodProv X(2)    Provincia   Provincia       CodDist X(2)    Distrito    Distrito        FlgCon  x(1)    Flag Control    Flag!Control        LugEnt2 x(60)   Lugar de entrega    Lugar de entrega        FlgAte  X   Estado  Estado      FchAte  99/99/9999  Fecha   Fecha   ?   Fecha de Despacho de Almacen    imptot2 ->>>,>>>,>>9.99 imptot2 0   ImpCto  ->>,>>>,>>9.99  ImpCto  ImpCto  0   AcuBon  ->>>,>>>,>>9.99 AcuBon  AcuBon  0   NroCard x(8)    NroCard Nrocard     TipBon  99  TipBon  TipBon  0   CCo X(5)    Centro de Costo Centro!de Costo     Centro de Costo FlgEnv  Si/No   El pedido es para enviar?   No  puntos  ->>,>>9.99  puntos  0   mrguti  ->>,>>9.99  mrguti  0   Sede    x(5)    Sede        Libre_c01   x(60)   Libre_c01       Libre_c02   x(60)   Libre_c02       Libre_c03   x(60)   Libre_c03       Libre_c04   x(60)   Libre_c04       Libre_c05   x(60)   Libre_c05       Libre_d01   ->>>,>>>,>>9.99<<<  Libre_d01   0   Libre_d02   ->>>,>>>,>>9.99<<<  Libre_d02   0   Libre_f01   99/99/99    Libre_f01   ?   Libre_f02   99/99/99    Libre_f02   ?   FchCre  99/99/99    FchCre  ?   Libre_f03   99/99/99    Libre_f03   ?   Libre_f04   99/99/99    Libre_f04   ?   Libre_f05   99/99/99    Libre_f05   ?   FchCobranza 99/99/9999  Fecha Cobranza  ?   UsrCobranza x(10)   Usuario Cobranza        DivOri  x(5)    Origen  Origen      ImpPro  ->>>,>>>,>>9.99 ImpPro  0   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   GlosaImpDto2    x(30)   GlosaImpDto2        CodCaja X(10)   Codigo Caja Codigo!Caja     Dcto_Otros_Mot  x(20)   Dcto_Otros_Mot      Dcto_Otros_Factor   ->>,>>9.999999  Dcto_Otros_Factor   0   Dcto_Otros_VV   ->>>,>>>,>>9.999999 Dcto_Otros_VV   0   Dcto_Otros_PV   ->>>,>>>,>>9.999999 Dcto_Otros_PV   0   Lista_de_Precios    x(8)    Lista_de_Precios        TotalValorVentaNetoOpGravadas   >>>>>>>>>>>9.99 TotalValorVentaNetoOpGravadas   0   TotalValorVentaNetoOpGratuitas  >>>>>>>>>>>9.99 TotalValorVentaNetoOpGratuitas  0   TotalTributosOpeGratuitas   >>>>>>>>>>>9.99 TotalTributosOpeGratuitas   0   TotalIGV    >>>>>>>>>>>9.99 TotalIGV    0   TotalImpuestos  >>>>>>>>>>>9.99 TotalImpuestos  0   TotalValorVenta >>>>>>>>>>>9.99 TotalValorVenta 0   TotalPrecioVenta    >>>>>>>>>>>9.99 TotalPrecioVenta    0   DescuentosGlobales  >>>>>>>>>>>9.99 DescuentosGlobales  0   PorcentajeDsctoGlobal   >>9.99999   PorcentajeDsctoGlobal   0   MontoBaseDescuentoGlobal    >>>>>>>>>>>9.99 MontoBaseDescuentoGlobal    0   TotalValorVentaNetoOpNoGravada  >>>>>>>>>>>9.99 TotalValorVentaNetoOpNoGravada  0   TotalDocumentoAnticipo  >>>>>>>>>>>9.99 TotalDocumentoAnticipo  0   MontoBaseDsctoGlobalAnticipo    >>>>>>>>>>>9.99 MontoBaseDsctoGlobalAnticipo    0   PorcentajeDsctoGlobalAnticipo   >>9.99999   PorcentajeDsctoGlobalAnticipo   0   TotalDsctoGlobalesAnticipo  >>>>>>>>>>>9.99 TotalDsctoGlobalesAnticipo  0   MontoBaseICBPER >>>>>>>>>>>9.99 MontoBaseICBPER 0   TotalMontoICBPER    >>>>>>>>>>>9.99 TotalMontoICBPER    0   TotalValorVentaNetoOpExoneradas >>>>>>>>>>>9.99 TotalValorVentaNetoOpExoneradas 0   TotalVenta  >>>>>>>>>>>9.99 TotalVenta  0   �   2 B W � � ��  ���������   �               P   P�         �   �   ��  00000  �      �    �       �   �           � �           �            �������                                    �%        �%        �%        �%        �%        �%        �%        &        &        &        &        '&        /&        7&        ?&        G&        O&                �     i  i  i  i      i  i  i      i  i  i  i  i  i      i  i  i  i      i  i  i  i  i      i  i  i  i  i  j      i  i  i  i  i      i 	 i 
 i  i  i      i  i  i  i  i      i  i  i  i  i  i      i  i  i  i  i      i  i  i  i  i  i      i  i  i      i  i  i  i  i      i  i  i  i  i  i      i  i  i  i      i  i  i  i  i     	1 	 	 	 	) 	8 	 	 	' 	( 	 	E 	 	G 	 	 	 	c 	       "   )   0   7   >   E   L   S   Z   a   h   o   v   }   �   �   �   �   �   3  :  �   �   �   �   �   �   �   �   �   �   �   �           %  ,  A  H  O  W  ^  f  m  t  {  �  �  �  �  �  �  �   �  �  �  �  �  �  �  �  �  �  �  �        
    "  )  0  8  ?  C  J  Q  X  ]  g  q  {  �  �  �  �  �  �  �  �  �  �  �  �  �    
      .  @  N  \  m  �  �  �  �  �  �  �    &  ?  ^  u  �  �  �  �  �      ��                                                                                                                       ����                            �    4�  2                 �    �   0�  2                 �    �%         �%         �&   R�    �&   �m    �%   y�    �&    ��    undefined                                                               �       8�  �   l   H�    ��                  �����               ��                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       �
     B          assignFocusedWidget         �      �     "      LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    6      LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    H      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          ^      LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    j      LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    v      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    �      LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �      LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H          LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    (      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    5      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    I      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    W      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    g      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    x      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    d           p          �           �              � ߱        T  $  �   p
  ���                           u   ����  �             �   �           �   �          �   �            �             �          ,  �              � ߱            Z   ����    ��
                         u   ���� �             8  �           D  �          P  �          \  �          x  �          �  �              � ߱            Z   �����   ��                     �    �  p  �  4  �      4   �����      o   �       �                              �  �  NA  �  �  �  �  �               (    <    P    d    x  `  �  
`  �  $  �    �     �      $  �  `  ���                       �     
                    � ߱        ��    
  �  $      �      4   �����                4                      ��                                      �Г                         �  �      P  `      ,      4   ����,      $    �  ���                       |  @         h              � ߱                �  �      �      4   �����      $      ���                         @                        � ߱        assignPageProperty                              �  �      ��                  �  �  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   8                            ��                  ,           ��                            ����                            changePage                              $        ��                  �  �  <              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             $        ��                  �  �  <              D�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  T           ��                            ����                            constructObject                             P  8      ��                  �  �  h              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
  �             �  
             ��                �               �� 
                 �  
         ��                            ����                            createObjects                               �  �      ��                  �  �                p�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �  �      ��                  �  �                ԰                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  $           ��                            ����                            destroyObject                                        ��                  �  �  8              0,                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                         ��                  �  �  8              �u                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  P           ��                            ����                            initializeObject                                P  8      ��                  �  �  h              �m                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               `  H      ��                  �  �  x              �n                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               `  H      ��                  �  �  x              T�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            notifyPage                              �  p      ��                  �  �  �              `�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            passThrough                             �  �      ��                  �  �  �              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                �               ��                             ��                            ����                            removePageNTarget                                 �      ��                  �  �                  2                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  l             8  
             ��                  `           ��                            ����                            selectPage                              X   @       ��                  �  �  p               Ȑ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �            ��                            ����                            toolbar                             |!  d!      ��                  �  �  �!              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �!           ��                            ����                            viewObject                              �"  �"      ��                  �  �  �"              �s                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                �#  �#      ��                  �  �  �#              �u                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �#           ��                            ����                            disablePagesInFolder    
      <$      t$    k      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder T$      �$      �$    �      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  �$       %      4%    �      HANDLE, getCallerWindow %      <%      l%    �      HANDLE, getContainerMode    L%      t%      �%    �      CHARACTER,  getContainerTarget  �%      �%      �%    �      CHARACTER,  getContainerTargetEvents    �%      �%      0&    �      CHARACTER,  getCurrentPage  &      <&      l&    �      INTEGER,    getDisabledAddModeTabs  L&      x&      �&     	      CHARACTER,  getDynamicSDOProcedure  �&      �&      �&  !  	      CHARACTER,  getFilterSource �&       '      0'  "  1	      HANDLE, getMultiInstanceActivated   '      8'      t'  #  A	      LOGICAL,    getMultiInstanceSupported   T'      �'      �'  $  [	      LOGICAL,    getNavigationSource �'      �'      �'  %  u	      CHARACTER,  getNavigationSourceEvents   �'      (      D(  &  �	      CHARACTER,  getNavigationTarget $(      P(      �(  '  �	      HANDLE, getOutMessageTarget d(      �(      �(  (  �	      HANDLE, getPageNTarget  �(      �(      �(  )  �	      CHARACTER,  getPageSource   �(      )      4)  *  �	      HANDLE, getPrimarySdoTarget )      <)      p)  +  �	      HANDLE, getReEnableDataLinks    P)      x)      �)  ,  �	      CHARACTER,  getRunDOOptions �)      �)      �)  -  
      CHARACTER,  getRunMultiple  �)      �)      (*  .  !
      LOGICAL,    getSavedContainerMode   *      4*      l*  /  0
      CHARACTER,  getSdoForeignFields L*      x*      �*  0  F
      CHARACTER,  getTopOnly  �*      �*      �*  1 
 Z
      LOGICAL,    getUpdateSource �*      �*       +  2  e
      CHARACTER,  getUpdateTarget  +      ,+      \+  3  u
      CHARACTER,  getWaitForObject    <+      h+      �+  4  �
      HANDLE, getWindowTitleViewer    |+      �+      �+  5  �
      HANDLE, getStatusArea   �+      �+      ,  6  �
      LOGICAL,    pageNTargets    �+       ,      P,  7  �
      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject 0,      �,      �,  8  �
      LOGICAL,INPUT h HANDLE  setCallerProcedure  �,      �,      -  9  �
      LOGICAL,INPUT h HANDLE  setCallerWindow �,      -      L-  :  �
      LOGICAL,INPUT h HANDLE  setContainerMode    ,-      d-      �-  ;  �
      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  x-      �-      �-  <  
      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �-      .      H.  =        LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  (.      d.      �.  >  ,      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  |.      �.      /  ?  C      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �.      $/      T/  @  Z      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  4/      t/      �/  A  j      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �/      �/      0  B  }      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �/      40      p0  C  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource P0      �0      �0  D  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �0      �0      41  E  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget 1      X1      �1  F  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget l1      �1      �1  G  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �1       2      02  H        LOGICAL,INPUT pcObject CHARACTER    setPageSource   2      T2      �2  I        LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget d2      �2      �2  J  $      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �2       3      83  K  8      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget 3      d3      �3  L  M      LOGICAL,INPUT phObject HANDLE   setRunDOOptions t3      �3      �3  M  ]      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �3      4      84  N  m      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   4      \4      �4  O  |      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields t4      �4      �4  P  �      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �4       5      L5  Q 
 �      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource ,5      l5      �5  R  �      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget |5      �5      �5  S  �      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    �5      6      H6  T  �      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    (6      h6      �6  U  �      LOGICAL,INPUT phViewer HANDLE   getObjectType   �6      �6      �6  V  �      CHARACTER,  setStatusArea   �6      �6      ,7  W        LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �7  �7      ��                  T  U  �7                                  O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �8  �8      ��                  W  X  �8              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �9  �9      ��                  Z  [   :              `�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �:  �:      ��                  ]  ^  ;              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �;  �;      ��                  `  b  <              �G                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  $<           ��                            ����                            getAllFieldHandles  7      �<      �<  X        CHARACTER,  getAllFieldNames    �<      �<       =  Y  &      CHARACTER,  getCol  �<      =      4=  Z  7      DECIMAL,    getDefaultLayout    =      @=      t=  [  >      CHARACTER,  getDisableOnInit    T=      �=      �=  \  O      LOGICAL,    getEnabledObjFlds   �=      �=      �=  ]  `      CHARACTER,  getEnabledObjHdls   �=       >      4>  ^  r      CHARACTER,  getHeight   >      @>      l>  _ 	 �      DECIMAL,    getHideOnInit   L>      x>      �>  `  �      LOGICAL,    getLayoutOptions    �>      �>      �>  a  �      CHARACTER,  getLayoutVariable   �>      �>      (?  b  �      CHARACTER,  getObjectEnabled    ?      4?      h?  c  �      LOGICAL,    getObjectLayout H?      t?      �?  d  �      CHARACTER,  getRow  �?      �?      �?  e  �      DECIMAL,    getWidth    �?      �?      @  f  �      DECIMAL,    getResizeHorizontal �?      @      P@  g  �      LOGICAL,    getResizeVertical   0@      \@      �@  h        LOGICAL,    setAllFieldHandles  p@      �@      �@  i        LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �@      �@      $A  j  )      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    A      DA      xA  k  :      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    XA      �A      �A  l  K      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �A      �A       B  m  \      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions     B      @B      tB  n  j      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout TB      �B      �B  o  {      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �B      �B       C  p  �      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical    C      LC      �C  q  �      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated `C      �C      �C  r  �      LOGICAL,    getObjectSecured    �C      �C      D  s  �      LOGICAL,    createUiEvents  �C      (D      XD  t  �      LOGICAL,    bindServer                              �D  �D      ��                  D  E  E              dT                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �E  �E      ��                  G  H  F              @�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                              G  �F      ��                  J  K  G              8�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                H  �G      ��                  M  N   H              PY                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              I  �H      ��                  P  Q  ,I              �Y                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             J  J      ��                  S  T  4J              �Z                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                              K  K      ��                  V  X  8K              \C                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 PK  
         ��                            ����                            startServerObject                               PL  8L      ��                  Z  [  hL              t                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                TM  <M      ��                  ]  _  lM              �t                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �M           ��                            ����                            getAppService   8D      �M      N  u  �      CHARACTER,  getASBound  �M      (N      TN  v 
 �      LOGICAL,    getAsDivision   4N      `N      �N  w  �      CHARACTER,  getASHandle pN      �N      �N  x        HANDLE, getASHasStarted �N      �N       O  y        LOGICAL,    getASInfo   �N      O      8O  z 	 (      CHARACTER,  getASInitializeOnRun    O      DO      |O  {  2      LOGICAL,    getASUsePrompt  \O      �O      �O  |  G      LOGICAL,    getServerFileName   �O      �O      �O  }  V      CHARACTER,  getServerOperatingMode  �O      P      <P  ~  h      CHARACTER,  runServerProcedure  P      HP      |P          HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   \P      �P      �P  �  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �P      Q      HQ  �  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle (Q      lQ      �Q  �  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   xQ      �Q      �Q  � 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �Q      R      <R  �  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  R      `R      �R  �  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   pR      �R      �R  �  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �R      S      @S  �  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �S  �S      ��                  "  &  T              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  `T             ,T  
             ��   �T             TT               �� 
                 |T  
         ��                            ����                            addMessage                              tU  \U      ��                  (  ,  �U              3                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �U             �U               ��    V             �U               ��                  �U           ��                            ����                            adjustTabOrder                              �V  �V      ��                  .  2  W              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  TW              W  
             �� 
  |W             HW  
             ��                  pW           ��                            ����                            applyEntry                              hX  PX      ��                  4  6  �X              hh                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �X           ��                            ����                            changeCursor                                �Y  |Y      ��                  8  :  �Y              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �Y           ��                            ����                            createControls                              �Z  �Z      ��                  <  =  �Z              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �[  �[      ��                  ?  @  �[              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �\  �\      ��                  B  C  �\              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �]  �]      ��                  E  F  �]               %                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �^  �^      ��                  H  I  �^              �%                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �_  �_      ��                  K  L  �_              X�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �`  �`      ��                  N  O  �`              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �a  �a      ��                  Q  V  �a              �y                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  Hb             b  
             ��   pb             <b               ��   �b             db               ��                  �b           ��                            ����                            modifyUserLinks                             �c  pc      ��                  X  \  �c              p6                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �c             �c               ��   d             �c               �� 
                 d  
         ��                            ����                            removeAllLinks                              e  �d      ��                  ^  _  e              �n                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              f  �e      ��                  a  e  f              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  hf             4f  
             ��   �f             \f               �� 
                 �f  
         ��                            ����                            repositionObject                                �g  lg      ��                  g  j  �g              @�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �g             �g               ��                  �g           ��                            ����                            returnFocus                             �h  �h      ��                  l  n  �h              Ԩ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 i  
         ��                            ����                            showMessageProcedure                                j  �i      ��                  p  s   j              l�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   lj             8j               ��                  `j           ��                            ����                            toggleData                              Xk  @k      ��                  u  w  pk                                  O   ����    e�          O   ����    R�          O   ����    ��            ��                  �k           ��                            ����                            viewObject                              �l  hl      ��                  y  z  �l              H}                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage   S      �l      m  � 
 _      LOGICAL,    assignLinkProperty  �l      (m      \m  �  j      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   <m      �m      �m  �  }      CHARACTER,  getChildDataKey �m      �m       n  �  �      CHARACTER,  getContainerHandle   n      ,n      `n  �  �      HANDLE, getContainerHidden  @n      hn      �n  �  �      LOGICAL,    getContainerSource  |n      �n      �n  �  �      HANDLE, getContainerSourceEvents    �n      �n       o  �  �      CHARACTER,  getContainerType     o      ,o      `o  �  �      CHARACTER,  getDataLinksEnabled @o      lo      �o  �  �      LOGICAL,    getDataSource   �o      �o      �o  �        HANDLE, getDataSourceEvents �o      �o      p  �         CHARACTER,  getDataSourceNames  �o      $p      Xp  �  4      CHARACTER,  getDataTarget   8p      dp      �p  �  G      CHARACTER,  getDataTargetEvents tp      �p      �p  �  U      CHARACTER,  getDBAware  �p      �p      q  � 
 i      LOGICAL,    getDesignDataObject �p      q      Lq  �  t      CHARACTER,  getDynamicObject    ,q      Xq      �q  �  �      LOGICAL,    getInstanceProperties   lq      �q      �q  �  �      CHARACTER,  getLogicalObjectName    �q      �q      r  �  �      CHARACTER,  getLogicalVersion   �q       r      Tr  �  �      CHARACTER,  getObjectHidden 4r      `r      �r  �  �      LOGICAL,    getObjectInitialized    pr      �r      �r  �  �      LOGICAL,    getObjectName   �r      �r      s  �  �      CHARACTER,  getObjectPage   �r      s      Ls  �  	      INTEGER,    getObjectParent ,s      Xs      �s  �        HANDLE, getObjectVersion    hs      �s      �s  �  '      CHARACTER,  getObjectVersionNumber  �s      �s      t  �  8      CHARACTER,  getParentDataKey    �s      t      Ht  �  O      CHARACTER,  getPassThroughLinks (t      Tt      �t  �  `      CHARACTER,  getPhysicalObjectName   ht      �t      �t  �  t      CHARACTER,  getPhysicalVersion  �t      �t      u  �  �      CHARACTER,  getPropertyDialog   �t      u      Lu  �  �      CHARACTER,  getQueryObject  ,u      Xu      �u  �  �      LOGICAL,    getRunAttribute hu      �u      �u  �  �      CHARACTER,  getSupportedLinks   �u      �u      v  �  �      CHARACTER,  getTranslatableProperties   �u      v      Lv  �  �      CHARACTER,  getUIBMode  ,v      Xv      �v  � 
 �      CHARACTER,  getUserProperty dv      �v      �v  �        CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �v      �v       w  �        CHARACTER,INPUT pcPropList CHARACTER    linkHandles  w      Hw      tw  �  *      CHARACTER,INPUT pcLink CHARACTER    linkProperty    Tw      �w      �w  �  6      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �w      x      0x  �  C      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   x      �x      �x  �  O      CHARACTER,INPUT piMessage INTEGER   propertyType    �x      �x       y  �  ]      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages   y      Hy      xy  �  j      CHARACTER,  setChildDataKey Xy      �y      �y  �  y      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �y      �y      z  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  �y      0z      dz  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    Dz      �z      �z  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �z      �z      {  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   �z      @{      p{  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents P{      �{      �{  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �{      �{       |  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget    |      H|      x|  �        LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents X|      �|      �|  �        LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �|      �|       }  � 
 3      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject  }      @}      t}  �  >      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    T}      �}      �}  �  R      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �}      �}      $~  �  c      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ~      H~      �~  �  y      LOGICAL,INPUT c CHARACTER   setLogicalVersion   `~      �~      �~  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   �~      �~      $  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent       D      t  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    T      �      �  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �      �      $�  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks �      L�      ��  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   `�      ��      ؀  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ��      ,�  �  
      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      P�      ��  �        LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   `�      ��      ܁  �  -      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��       �      <�  �  ?      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  �      `�      ��  � 
 Y      LOGICAL,INPUT pcMode CHARACTER  setUserProperty l�      ��      ܂  �  d      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      �      H�  �  t      LOGICAL,INPUT pcMessage CHARACTER   Signature   (�      l�      ��  � 	 �      CHARACTER,INPUT pcName CHARACTER    ��    �  ؃  T�      D      4   ����D                d�                      ��                  �  �                  �                       �  �        �  ��  ��      T      4   ����T                �                      ��                  �  �                  (                       �  ��  �    �  (�  ��      h      4   ����h                ��                      ��                  �  �                  �                       �  8�         �                                  <     
                    � ߱        8�  $  �  ��  ���                           $  �  d�  ���                       �                         � ߱        ��    �  ��  (�      �      4   �����                8�                      ��                  �  �	                  X                       �  ��  l�  o   �      ,                                 ć  $   �  ��  ���                         @         �              � ߱        ؇  �   �  ,      �  �   �  �       �  �   �        �  �   �  �      (�  �   �  �      <�  �   �  p      P�  �   �  �      d�  �   �  (	      x�  �   �  �	      ��  �   �  
      ��  �   �  �
      ��  �   �        Ȉ  �   �  �      ܈  �   �  �      ��  �   �  <      �  �   �  �      �  �   �  �      ,�  �   �  `      @�  �   �  �      T�  �   �        h�  �   �  �      |�  �   �         ��  �   �  |      ��  �   �  �      ��  �   �  l      ̉  �   �  �      ��  �   �  T      �  �   �  �      �  �   	        �  �   	  @      0�  �   	  �      D�  �   	  �      X�  �   	  ,      l�  �   	  h      ��  �   	  �      ��  �   		         ��  �   
	  \      ��  �   	  �      Њ  �   	  �      �  �   	        ��  �   	  L      �  �   	  �       �  �   	  �      4�  �   	             �   	  <                      `�          ̋  ��      ��                  �	  �	  �              L                    O   ����    e�          O   ����    R�          O   ����    ��      �     
  
       
       (                     8                         � ߱        ��  $ �	  ��  ���                           O   �	  ��  ��  x               ��          �  ��    ،                                             ��                            ����                                �6      H�      ��     6      �                      V ��  �                     \�    �	  ��  4�      �      4   �����                D�                      ��                  �	  �
                  D                       �	  ȍ  X�  �    
  �      l�  �   
  X      ��  �   
  �      ��  �   
  P      ��  �   
  �      ��  �   
  H      Ў  �   
  �      �  �   
  8      ��  �   
  �      �  �   	
  0       �  �   

  �      4�  �   
         H�  �   
  �          �   
        4�    �
  x�  �      �      4   �����                �                      ��                  �
                    �5                       �
  ��  �  �   �
  �      ,�  �   �
  \       @�  �   �
  �       T�  �   �
  L!      h�  �   �
  �!      |�  �   �
  4"      ��  �   �
  �"      ��  �   �
  $#      ��  �   �
  �#      ̐  �   �
  $      ��  �   �
  �$      ��  �   �
  �$      �  �   �
  p%      �  �   �
  �%      0�  �   �
  h&      D�  �   �
  �&      X�  �   �
  `'      l�  �   �
  �'      ��  �   �
  X(      ��  �   �
  �(      ��  �   �
  P)      ��  �   �
  �)      Б  �   �
  H*      �  �   �
  �*      ��  �   �
  @+      �  �   �
  �+       �  �   �
  8,          �   �
  �,      P�    )  P�  ̒      -      4   ����-                ܒ                      ��                  *  �                  H8                       *  `�  �  �   -  |-      �  �   .  �-      �  �   /  t.      ,�  �   0  �.      @�  �   2  \/      T�  �   3  �/      h�  �   5  D0      |�  �   6  �0      ��  �   7  �0      ��  �   8  01      ��  �   9  l1      ̓  �   :  �1      ��  �   ;  T2      ��  �   <  �2      �  �   >  D3      �  �   ?  �3      0�  �   @  ,4      D�  �   A  �4      X�  �   B  $5      l�  �   C  `5      ��  �   E  �5      ��  �   F  H6      ��  �   G  �6      ��  �   H  �6      Д  �   I  47      �  �   J  �7      ��  �   K  �7      �  �   L  (8       �  �   M  d8      4�  �   N  �8      H�  �   O  �8      \�  �   P  9      p�  �   Q  T9      ��  �   S  �9      ��  �   T  :      ��  �   U  @:      ��  �   V  |:      ԕ  �   W  �:      �  �   X  �:      ��  �   Y  0;      �  �   Z  l;      $�  �   [  �;      8�  �   \  T<      L�  �   ]  �<      `�  �   ^  <=      t�  �   _  �=      ��  �   `  4>      ��  �   a  �>      ��  �   b  ,?      Ė  �   c  �?      ؖ  �   d  $@      �  �   e  `@       �  �   f  �@      �  �   g  A      (�  �   h  TA      <�  �   i  �A          �   j  B      ��  $  �  |�  ���                       lB     
                    � ߱        @�       ė  ԗ      xB      4   ����xB      /   !   �     �                          3   �����B            0�                      3   �����B  ��    *  \�  ؘ  Ĝ  �B      4   �����B  	              �                      ��             	     +  �                                          +  l�  ��  �   /  $C      T�  $  0  (�  ���                       PC     
                    � ߱        h�  �   1  pC      ��  $   3  ��  ���                       �C  @         �C              � ߱        |�  $  6  �  ���                       �C                         � ߱        `D     
  
       
       �D                     ,F  @        
 �E              � ߱        �  V   @  �  ���                        8F                     lF                     �F                         � ߱        ��  $  \  ��  ���                       hG     
  
       
       �G                     4I  @        
 �H              � ߱        ,�  V   n  8�  ���                        @I     
  
       
       �I                     K  @        
 �J              � ߱            V   �  ț  ���                        
              ��                      ��             
     �  N                  �r                       �  X�  K     
  
       
       �K                     �L  @        
 �L          HM  @        
 M          �M  @        
 hM          N  @        
 �M              � ߱            V   �  Ԝ  ���                        adm-clone-props @�  ��              �     7     `                          \  �"                     start-super-proc    ȝ  $�  �           �     8                                  �"                     ,�    f  ��  ��      �Q      4   �����Q      /   g  �     ��                          3   �����Q            �                      3   �����Q  ��  $  �  X�  ���                       �Q                         � ߱        @�    �  ��  �  ��   R      4   ���� R                ��                      ��                  �  �                  �                       �  ��  R                     (R                     <R                         � ߱            $  �  ,�  ���                             �  ؠ  �      TR      4   ����TR  tR                         � ߱            $  �  �  ���                       <�    �  \�  l�  ġ  �R      4   �����R      $  �  ��  ���                       �R                         � ߱            �   �  �R      �R     
  
       
       xS                     �T  @        
 �T              � ߱        h�  V   �  ء  ���                        |�  �     �T      �    �  ��  ��      U      4   ����U      /   �  Ԣ     �                          3   ����$U            �                      3   ����DU  У  $  �  @�  ���                       `U                         � ߱        �U     
  
       
       V                     XW  @        
 W              � ߱        ��  V   �  l�  ���                        ܦ      �  ��      dW      4   ����dW                ��                      ��                                      �p                         (�      g     ��         ����                           ��          T�  <�      ��                        l�              8q                    O   ����    e�          O   ����    R�          O   ����    ��          /    ��     ��  �W                      3   ����tW  �     
   �                      3   �����W         
   �                      3   �����W    ��                              ��                          ����                                        Ф              9       �                      g                               �  g     ��          ��	��                           ��          ��  t�      ��                      ��              �q                    O   ����    e�          O   ����    R�          O   ����    ��          /    �     ��  �W                      3   �����W            �                      3   �����W    ��                              ��                          ����                                        �              :      (�                      g                               �  g     ��          ��	��                           ĩ          ��  |�      ��                      ��              pr                    O   ����    e�          O   ����    R�          O   ����    ��          /    �      �  X                      3   �����W             �                      3   ����X    ��                              ��                          ����                                        �              ;      0�                      g                               L�    3  �  ��      (X      4   ����(X                ��                      ��                  4  S                  5                       4  �   �  /   5  ��     Ы                          3   ����8X            �                      3   ����XX  ��  /  7  ,�     <�  �X                      3   ����tX  l�     
   \�                      3   �����X  ��        ��                      3   �����X  ̬        ��                      3   �����X            �                      3   �����X  $�    ?  �  (�       Y      4   ���� Y      /  E  T�     d�  �Y                      3   ����hY  ��     
   ��                      3   �����Y  ĭ        ��                      3   �����Y  ��        �                      3   �����Y            �                      3   �����Y        K  @�  P�      �Y      4   �����Y      /  N  |�     ��  DZ                      3   ����$Z  ��     
   ��                      3   ����LZ  �        ܮ                      3   ����TZ  �        �                      3   ����hZ            <�                      3   �����Z  �    W  h�  �      �Z      4   �����Z                ��                      ��                  X  [                  `�t                       X  x�      g   Y  �         ����        �Z                  ԰          ��  ��      ��                  Z      ��              ĭt                    O   ����    e�          O   ����    R�          O   ����    ��          /  Z   �     �  �Z                      3   �����Z  @�     
   0�                      3   �����Z         
   `�                      3   �����Z    ��                            ����                                         �              <      p�                      g                               ��     _  �Z                                     [     
  
       
       �[                     �\  @        
 �\              � ߱        4�  V   �  @�  ���                        �\     
  
       
       h]                     �^  @        
 x^              � ߱        `�  V   �  в  ���                        �    +  |�  ��      �^      4   �����^      $   ,  ��  ���                       ,_  @         _              � ߱        ��  g   d  ��         ��\�        @_  ��\�        L_                  ش          ��  ��      ��                  e  j  ��              ��t                    O   ����    e�          O   ����    R�          O   ����    ��            i  ��  �      X_      4   ����X_      O  i  ������  l_    ��                            ����                                        $�              =      �                      g                               d�  g   q  е         �6�         �_                  ��          h�  P�      ��                  r  w  ��              ,�t                    O   ����    e�          O   ����    R�          O   ����    ��      ��    u  �_  }          O  v  ������  �_    ��                            ����                                        �              >      ȶ                      g                               �  g     |�         �"��                           D�          �  ��      ��                  �  �  ,�              ��t                    O   ����    e�          O   ����    R�          O   ����    ��            �  �_  }        ��                              ��                          ����                                        ��              ?      \�                      g                               �  g   �  0�         �"��        
                   ��          ȹ  ��      ��                 �  �  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��      �    �  �  ��      �_      4   �����_                ��                      ��                  �  �                  t��                       �  $�  �  	  �  Ժ                                        3   �����_  ��    �  �_           O  �  ������  `      /   �  @�                                 3   ����$`    ��                              ��                          ����                                        D�              @      P�                      g                                �  g   �  $�         �"��                           �          ��  ��      ��                 �  �  Լ              ��                    O   ����    e�          O   ����    R�          O   ����    ��      �    �  �  ��      8`      4   ����8`                ��                      ��                  �  �                  @��                       �  �  ؽ  	  �  Ƚ                                        3   ����``  �    �  l`           O  �  ������  �`      /   �  4�                                 3   �����`    ��                              ��                          ����                                        8�              A      D�                      g                               ��  g   �  �         �4T�                            �          ��  ��      ��                  �  �  ȿ              ܫ�                    O   ����    e�          O   ����    R�          O   ����    ��      8�  $  �  �  ���                       �`                         � ߱        t�  /   �  d�                                 3   �����`  �  s   �  ��                 �              ��  �       ��                            7   ����           ��                     �            l�                  6   �         ��   ��                    �            l�                                                                ��  ��                                   @            ��   ��          a  a   a  ,a          ��      s   �  <�                ��              h�  ��       ��                            7   ����           ��                     �            �                  6   �         ,�   ��                    �            �                                                                t�  h�                                   @            H�   X�          8a  Da  Pa  \a          ��    ��                              ��                          ����                            �        2                 �    �       2                 �                ,�              B      ��             �      g                               ��  g   �  ��         � P�                           ��          `�  H�      ��H�                �  �  x�              �t                    O   ����    e�          O   ����    R�          O   ����    ��      d�  A  �        ��   ��         ��  �a                                        ha   ta   �a                 P�  D�           �a  �a  �a           �a  �a  �a         �            �   ,�          �  ��  ��      b      4   ����b   b                            � ߱            Z   �  ��   �                          ��                              ��                          ����                                              ��              C      ��                      g                               |�  g   �  ��         �  �                           ��          \�  D�      ����               �  �  t�              �u                    O   ����    e�          O   ����    R�          O   ����    ��      ��    �  ��  ��      ,b      4   ����,b      O   �  ��  ��  Tb  ��  A  �        4�   ��          �  �b                                        hb   tb                   ��  x�           �b  �b           �b  �b         �            P�   d�    ��    �  ��  0�      �b      4   �����b                @�                      ��                  �  �                  �Mt                       �  ��  ��  	  �  t�                                        3   �����b      O  �  ������  �b  ��  $  �  ��  ���                       c                         � ߱        x�  $   �   �  ���                       $c  @         c              � ߱        Dc                            � ߱        ��  Z   �  L�   �                        ��  $   �  ��  ���                       dc  @         Pc              � ߱        8�  /   �  (�                                 3   ����xc  ��  s   �  d�                 ��              ��  ��       ��                            7   ����           ��                     �            0�                  6   �         T�   ��                    �            0�                                                                ��  ��                                   @            p�   ��          �c  �c  �c  �c          ��      s   �   �                l�              ,�  |�       ��                            7   ����           ��                     �            ��                  6   �         ��   ��                    �            ��                                                                8�  ,�                                   @            �   �          �c  �c  �c  �c          L�    ��                              ��                          ����                                  �        2                 �    �       2                 �                ��              D      p�             ��      g                                     �  ��  �      �c      4   �����c                ��                      ��                  �  *                  P&u                       �  ��  d  @                     4d  @          d          \d  @         Hd              � ߱        ��  $   �  $�  ���                       ��  g     ��         �nT�      }                      ��          d�  L�      ��                    
  |�              �&u                    O   ����    e�          O   ����    R�          O   ����    ��      ��  /    ��                                 3   ����hd          ��  ��      �d      4   �����d      O  	  ������  �d    ��                            ����                                        ��              E      �                      g                               ��  g     ��         �!(�         �d                  ��          `�  H�      ��                      x�              @'u                    O   ����    e�          O   ����    R�          O   ����    ��      �d  @                         � ߱            $    ��  ���                         ��                            ����                                        ��              F      ��                      g                               ��  /     ��                                 3   �����d          ��  X�      �d      4   �����d                ��                      ��                    (                  h2u                         ��                �          ��  ��      ��                   &                  �2u                         h�      O       ��          O       ��      P�  /   #  @�                                 3   ����e        $  l�  |�      4e      4   ����4e      k   %  ��              }       n        �   adm-create-objects  8�  ��                      G      �                               =%                     Carga-Temporales    ��   �                      H      <	                              a%                     disable_UI  4�  ��                      I      <                              r%  
                   enable_UI   ��  ��                      J      x             �              }%  	                   Enviar  �  `�                      K      `
             �
              �%                     exitObject  h�  ��                      L      �                               �%  
                   initializeObject    ��  ,�                   M     p                          l  �%                     Regresar    @�  ��                      N      `
             �
              �%                      ��   ����� �� �     �     ���  �              X�  8   ����    h�  8   ����    x�  8   ����   ��  8   ����   ��  8   ����   ��  8   ����   ��    ��  8   ����   ��  8   ����         ��  8   ����   ��  8   ����       8   ����       8   ����       �  $�      toggleData  ,INPUT plEnabled LOGICAL    �  P�  h�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  @�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  0�  <�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE  �  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  �  0�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    �  ��  ��      hideObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  ��  �      displayLinks    ,   ��  �  (�      createControls  ,   �  <�  L�      changeCursor    ,INPUT pcCursor CHARACTER   ,�  x�  ��      applyEntry  ,INPUT pcField CHARACTER    h�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  �  $�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER �  |�  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE l�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  �  $�      startServerObject   ,    �  8�  H�      runServerObject ,INPUT phAppService HANDLE  (�  t�  ��      restartServerObject ,   d�  ��  ��      initializeServerObject  ,   ��  ��  ��      disconnectObject    ,   ��  ��  �      destroyServerObject ,   ��  �  $�      bindServer  ,   �  8�  H�      processAction   ,INPUT pcAction CHARACTER   (�  t�  ��      enableObject    ,   d�  ��  ��      disableObject   ,   ��  ��  ��      applyLayout ,   ��  ��  ��      viewPage    ,INPUT piPageNum INTEGER    ��  �   �      viewObject  ,   �  4�  <�      toolbar ,INPUT pcValue CHARACTER    $�  h�  t�      selectPage  ,INPUT piPageNum INTEGER    X�  ��  ��      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  ��  ��      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  D�  P�      notifyPage  ,INPUT pcProc CHARACTER 4�  x�  ��      initPages   ,INPUT pcPageList CHARACTER h�  ��  ��      initializeVisualContainer   ,   ��  ��  ��      hidePage    ,INPUT piPageNum INTEGER    ��  �  (�      destroyObject   ,   �  <�  H�      deletePage  ,INPUT piPageNum INTEGER    ,�  t�  ��      createObjects   ,   d�  ��  ��      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  �  (�      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  �  X�  d�      changePage  ,   H�  x�  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
   � �   �%              � �     %              %              %              %              "      "      "  )    4         "      "      "      "      "      "  )    4         "      "      "          �     }        �G� @   �G%              � D  &   %       	 %       %        %       %        %       %               %               %               %              %              %              %               %              
�        
"   
 �
�    
"   
 �
"   
 �    �             �             
"   
   �        \         �     }        �%              
"   
 �
"   
 �    �        �     �        �    
"   
   �        �         �     }        �%              � 
"    
 �%              � �  �         �      T     @     $              
�    � �   �     
"   
 � �   �     
�             �G                      
�            � �   
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 ��                1� �  
 �� �   �%               o%   o           � �    �
"   
 ��           �    1� �   �� �   �%               o%   o           � �   �
"   
 ��               1� �  
 �� �   �%               o%   o           � �   �
"   
 ��           |    1� �   �� �   �%               o%   o           � �   �
"   
 ��           �    1� �   �� �   �%               o%   o           � �   �
"   
 ��           d    1�    ��     �%               o%   o           %               
"   
 ��          �    1� (   �� 8     
"   
 ��           	    1� ?   �� �   �%               o%   o           � R  e �
"   
 ��           �	    1� �   �� �   �%               o%   o           � �  [ �
"   
 ��           
    1� #   ��     �%               o%   o           %               
"   
 ��           �
    1� 3   ��     �%               o%   o           %               
"   
 ��           �
    1� E   ��     �%               o%   o           %              
"   
 ��          x    1� R   ��       
"   
 ��           �    1� a  
 ��     �%               o%   o           %               
"   
 ��           0    1� l   �� �   �%               o%   o           � �    �
"   
 ��          �    1� t   �� 8     
"   
 ��           �    1� �   �� �   �%               o%   o           � �  t �
"   
 ��          T    1�   
 �� 8     
"   
 ��           �    1�    �� �   �%               o%   o           � +  � �
"   
 ��               1� �   �� �   �%               o%   o           � �    �
"   
 ��           x    1� �  
 �� �   �%               o%   o           %               
"   
 �           �    1� �   �     �%               o%   o           %               
"   
 �           p    1� �   � �   �%               o%   o           � �    
"   
 �           �    1� �   � �   �%               o%   o           o%   o           
"   
 �           `    1�   
 � �   �%               o%   o           � �    
"   
 �           �    1�    � #  	 �%               o%   o           � -  / 
"   
 ��          H    1� ]   �� #  	   
"   
 �           �    1� o   � #  	 �o%   o           o%   o           � �    
"   
 ��          �    1� �   �� #  	   
"   
 ��           4    1� �   �� #  	 �o%   o           o%   o           � �    �
"   
 ��          �    1� �   ��       
"   
 ��          �    1� �   �� #  	   
"   
 ��               1� �   �� #  	   
"   
 ��          \    1� �   �� #  	   
"   
 �           �    1� �   �     �o%   o           o%   o           %              
"   
 ��              1� �   �� #  	   
"   
 ��          P    1� �  
 ��      
"   
 ��          �    1� 	   �� #  	   
"   
 ��          �    1�    �� #  	   
"   
 ��              1� +   �� #  	   
"   
 ��          @    1� @   �� #  	   
"   
 ��          |    1� O  	 �� #  	   
"   
 ��          �    1� Y   �� #  	   
"   
 ��          �    1� l   �� #  	   
"   
 �           0    1� �   � �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"  
 
   
"  
 
 
"  
 
   
"  
 
 �(�  L ( l       �        �    �� �   � P   �            �@    
� @  , 
�           �� �     p�               �L
�    %              � 8          � $         � �          
�    � �     
"  
 
 �� @  , 
�       ,    �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� �  
 � �   �%               o%   o           � �    
"   
 �           L    1� �  
 � �   �%               o%   o           o%   o           
"   
 �           �    1� �   � 8   �%               o%   o           o%   o           
"   
 �           D    1� �   �     �%               o%   o           %               
"   
 �           �    1� �   �     �%               o%   o           %               
"   
 ��           <    1� �   �� �   �%               o%   o           � �    
"   
 �           �    1� �   �     �%               o%   o           %              
"   
 �           ,    1�    �     �%               o%   o           o%   o           
"   
 �           �    1�    � �   �%               o%   o           o%   o           
"   
 �           $    1� *  	 � �   �%               o%   o           � �    
"   
 �           �    1� 4   � �   �%               o%   o           o%   o           
"   
 �               1� H   � �   �%               o%   o           o%   o           
"   
 �           �    1� W   �     �%               o%   o           %               
"   
 �               1� g   �     �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� s   � #  	 �%               o%   o           � �    
"   
 �           P     1� �   � #  	 �%               o%   o           � �    
"   
 �           �     1� �   �     �%               o%   o           %               
"   
 ��           @!    1� �   �� #  	 �%               o%   o           � �    
"   
 �           �!    1� �   � #  	 �%               o%   o           � �    �
"   
 �           ("    1� �   �     �%               o%   o           %               
"   
 �           �"    1� �   � #  	 �%               o%   o           � �    
"   
 �           #    1� �   � #  	 �%               o%   o           � �    
"   
 �           �#    1� �   � #  	 �%               o%   o           � �    
"   
 �            $    1� �   � #  	 �%               o%   o           o%   o           
"   
 �           |$    1�    � #  	 �%               o%   o           � �    
"   
 ��           �$    1�    �� #  	 �%               o%   o           � �    
"   
 �           d%    1�   	 �    �%               o%   o           %               
"   
 �           �%    1� )   �    �%               o%   o           %               
"   
 �           \&    1� 2   �     �%               o%   o           o%   o           
"   
 �           �&    1� C   �     �%               o%   o           o%   o           
"   
 �           T'    1� R   �     �%               o%   o           %               
"   
 �           �'    1� `   �     �%               o%   o           %               
"   
 �           L(    1� q   �     �%               o%   o           %               
"   
 ��           �(    1� �   �� �   �%               o%   o           %       
       
"   
 ��           D)    1� �   �� �   �%               o%   o           o%   o           
"   
 �           �)    1� �   � �   �%               o%   o           %              
"   
 �           <*    1� �   � �   �%               o%   o           o%   o           
"   
 �           �*    1� �   � �   �%               o%   o           %              
"   
 �           4+    1� �   � �   �%               o%   o           o%   o           
"   
 �           �+    1� �   � �   �%               o%   o           %              
"   
 �           ,,    1� �   � �   �%               o%   o           o%   o           
"   
 ��           �,    1� �   �� #  	 �%               o%   o           � �    P �L 
�H T   %              �     }        �GG %              
"   
 �           p-    1� �   � �   �%               o%   o           %               
"   
 �           �-    1�    � �   �%               o%   o           o%   o           
"   
 �           h.    1�    � �   �%               o%   o           � �    
"   
 �           �.    1� "   � �   �%               o%   o           � 8  - 
"   
 �           P/    1� f   � �   �%               o%   o           � �    
"   
 �           �/    1� }   � �   �%               o%   o           � �   
"   
 ��          80    1� �   �� 8     
"   
 �           t0    1� �   � �   �%               o%   o           � �    
"   
 ��          �0    1� �  
 �� 8     
"   
 ��          $1    1� �   �� 8     
"   
 �           `1    1� �   � #  	 �%               o%   o           � �    
"   
 �           �1    1� �   � �   �%               o%   o           � �    
"   
 �           H2    1�    � 8   �%               o%   o           o%   o           
"   
 �           �2    1�    � �   �%               o%   o           � '  ! 
"   
 �           83    1� I   � �   �%               o%   o           � �    
"   
 ��           �3    1� V   �� �   �%               o%   o           � i   
"   
 ��            4    1� x  	 �� �   �%               o%   o           o%   o           
"   
 �           �4    1� �   �     �%               o%   o           %               
"   
 ��          5    1� �   �� 8     
"   
 �           T5    1� �   � �   �%               o%   o           � �   
"   
 �           �5    1� �   � #  	 �%               o%   o           � �    
"   
 �           <6    1� �   � #  	 �%               o%   o           � �    
"   
 ��          �6    1� �   �� 8     
"   
 ��          �6    1� �   �� #  	   
"   
 ��           (7    1�     ��     �o%   o           o%   o           %               
"   
 ��          �7    1�     ��       
"   
 ��          �7    1� /    �� #  	   
"   
 ��          8    1� =    �� #  	   
"   
 ��          X8    1� P    �� #  	   
"   
 ��          �8    1� a    �� #  	   
"   
 ��          �8    1� r    �� #  	   
"   
 ��          9    1� �    �� 8     
"   
 �           H9    1� �    � �   �%               o%   o           � �   4 
"   
 ��          �9    1� �    �� 8     
"   
 ��          �9    1� �    �� 8     
"   
 ��          4:    1� �    �� 8     
"   
 ��          p:    1� 
!   �� #  	   
"   
 ��          �:    1� !   �� #  	   
"   
 ��          �:    1� 0!   �� #  	   
"   
 ��          $;    1� B!   ��       
"   
 �           `;    1� O!   � #  	 �%               o%   o           � �    
"   
 �           �;    1� ]!   � #  	 �%               o%   o           � �    
"   
 �           H<    1� i!   � #  	 �%               o%   o           � �    
"   
 �           �<    1� ~!   � #  	 �%               o%   o           � �    
"   
 �           0=    1� �!   �     �%               o%   o           %               
"   
 �           �=    1� �!   �     �%               o%   o           o%   o           
"   
 �           (>    1� �!   �     �%               o%   o           %               
"   
 �           �>    1� �!   �     �%               o%   o           %               
"   
 �            ?    1� �!   �     �%               o%   o           o%   o           
"   
 �           �?    1� �!   �     �%               o%   o           %               
"   
 ��          @    1� �!   �� #  	   
"   
 �           T@    1� "   �     �%               o%   o           %              
"   
 ��          �@    1� "   �� #  	   
"   
 ��          A    1� #"   �� #  	   
"   
 ��          HA    1� 2"  
 �� #  	   
"   
 �           �A    1� ="   � #  	 �%               o%   o           � �!   
"   
 �           �A    1� O"   � #  	 �%               o%   o           � �    
"   
    "    �%     start-super-proc ��%     adm2/smart.p ��P �L 
�H T   %              �     }        �GG %              
"   
   �       C    6� �     
"   
   
�        DC    8
"   
   �        dC    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"  
 
 �
"  
 
 �
"  
 
 �
"  
 
   (�  L ( l       �        �D    �� �   � P   �        �D    �@    
� @  , 
�       �D    �� �   �p�               �L
�    %              � 8      �D    � $         � �          
�    � �   �
"  
 
 �p� @  , 
�       �E    �� ?   �p�               �L"    , �   � �"   � �"   ��     }        �A      |    "      � �"   %              (<   \ (    |    �     }        �A� �"   �A"        "    �"      < "    �"    (    |    �     }        �A� �"   �A"    
�H T   %              �     }        �GG %              
"  
 
 �
"  
 
 �
"  
 
 �
"  
 
   (�  L ( l       �        �G    �� �   � P   �        �G    �@    
� @  , 
�       �G    �� �   �p�               �L
�    %              � 8      �G    � $         � �          
�    � �   �
"  
 
 �p� @  , 
�       �H    �� �  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"  
 
 �
"  
 
 �
"  
 
 �
"  
 
   (�  L ( l       �        �I    �� �   � P   �        �I    �@    
� @  , 
�       �I    �� �   �p�               �L
�    %              � 8      �I    � $         � �          
�    � �   �
"  
 
 �p� @  , 
�       �J    �� (   �p�               �L
"   
 , 
�H T   %              �     }        �GG %              
"  
 
   
"  
 
 
"  
 
   
"  
 
   (�  L ( l       �        dK    �� �   � P   �        pK    �@    
� @  , 
�       |K    �� �     p�               �L
�    %              � 8      �K    � $         � �          
�    � �     
"  
 
 �p� @  , 
�       �L    �� �  
 �p�               �L%     SmartWindow 
"  
 
   p� @  , 
�       �L    �� �     p�               �L%      WINDOW  
"  
 
  p� @  , 
�       \M    �� �    p�               �L%               
"  
 
  p� @  , 
�       �M    �� o    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
  (   � 
"   
 �    �        �N    �� �   �
"   
   � 8      �N    � $         � �          
�    � �   �
"   
   �        @O    �
"   
   �       `O    /
"   
   
"   
   �       �O    6� �     
"   
   
�        �O    8
"   
   �        �O    �
"   
   �       �O    �
"   
   p�    � �"   
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �P    �A"    �A
"   
   
�        Q    �@ � 
"   
 "      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p ��    � :#     
�    �     }        �%               %      Server  - �     }        �    "    � �    �%                   "    � �    �%      NONE    p�,  8         $     "            � T#   �
�    
�H T   %              �     }        �GG %              
"  
 
 �
"  
 
 �
"  
 
 �
"  
 
   (�  L ( l       �        HS    �� �   � P   �        TS    �@    
� @  , 
�       `S    �� �   �p�               �L
�    %              � 8      lS    � $         � �          
�    � �   �
"  
 
 �p� @  , 
�       |T    �� 4   �p�               �L"    , p�,  8         $     "            � b#   �
�     "    �%     start-super-proc ��%     adm2/visual.p ��   � �     � �#     � �#  e   
�H T   %              �     }        �GG %              
"  
 
 �
"  
 
 �
"  
 
 �
"  
 
   (�  L ( l       �        �U    �� �   � P   �        �U    �@    
� @  , 
�       �U    �� �   �p�               �L
�    %              � 8      �U    � $         � �          
�    � �   �
"  
 
 �p� @  , 
�       W    �� �   �p�               �L"    , � 
" 	   
 �%     contextHelp 
" 	   
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP ��%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %      initializeDataObjects 0 0   A    �    � 4$   
�    � F$   �A    �    � 4$     
�    � R$   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %     buildDataRequest ent0 A    �    � 4$   �
�    � o$   %     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
" 	   
 �
"   
 �%     contextHelp 
" 	   
   
�    
�    %               
�H T   %              �     }        �GG %              
"  
 
 �
"  
 
 �
"  
 
 �
"  
 
 (�  L ( l       �        X[    �� �   � P   �        d[    �@    
� @  , 
�       p[    �� �   �p�               �L
�    %              � 8      |[    � $         � �   �     
�    � �   �
"  
 
 �p� @  , 
�       �\    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"  
 
 �
"  
 
 �
"  
 
 �
"  
 
 �(�  L ( l       �        8]    �� �   � P   �        D]    �@    
� @  , 
�       P]    �� �   �p�               �L
�    %              � 8      \]    � $         � �   �     
�    � �   �
"  
 
 �p� @  , 
�       l^    �� �!   �p�               �L%              (        �     }        �G� @   �G� 
"   
 �
"   
   �        _    �%              
"   
 �
"   
 ��     }        �%               
"   
 �%      CLOSE   %               %      CLOSE       �            B� �$    B� �$     %      ENTRY   %               %      Enviar      �            B� �$    B� �$     %      ENTRY   %               %     Regresar ��T   %              �     }        B� �$   B%     Carga-Temporales �"      "   1   "      "      "      "   1   "      "      "     ��            B�            B&    &    &    &    &    &    0        %              %              %              *    "          �     }        B� �$    B%               "     ��     }        B&    &    &    &        %              %               *    � %     %               "      �            B    "    B� &%   B"      �     }         %               %     Carga-Temporales �"      "   1   "      "      "      "   1   "      "      � 
"   
 �
"   
 
"   
 ��        d    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � 3%  	   %               
"   
 �
�    %     createObjects    �     }        �%     initializeObject � �     }        �� Y%   �� ]%   �� _%   �� _%   �"     �"    �"    �&    &    &    &    &    &    �    �    p    L    0        %              %              %                  "      &        "      &        "  +    &        "  /    &    (        �     }        �G� @   �G� 
"   
 �
"   
   �     }        �
�    
"   
 �"    "    �"      "      "      "  	    
"   
 "      "   1   "      "      "      "   1   "      "      
"   
    *    %               "      "   1   "      "      "      "   1   "      "      %      CLOSE   %               "     �&    &        "    �� �$    �           "       � �$     "        H     4               "      � �%     "       � �$     "           "       "                  "       � �$     "       �            "      "       %     Carga-Temporales �%      SUPER    *    %               "      "   1   "      "      "      "   1   "      "                      �           �   l       ��                 �  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       PN     
                    � ߱              �  (  �      �N      4   �����N                �                      ��                  �  �                  �                       �  8  �  �  �  �N            �  �  `      LO      4   ����LO                p                      ��                  �  �                  	                       �  �  �  o   �      ,                                 �  �   �  lO      �  �   �  �O      $  $  �  �  ���                       �O     
                    � ߱        8  �   �  �O      L  �   �  P      `  �   �  $P          $   �  �  ���                       TP  @         @P              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                   G  �               `
                    O   ����    e�          O   ����    R�          O   ����    ��      �"                      �          �  $      ���                       �P     
                    � ߱                  �  �                      ��                                       �                       4      4   �����P      $    �  ���                       Q     
                    � ߱        �      4  D      (Q      4   ����(Q      /    p                               3   ����<Q  �  �   8  HQ          O   E  ��  ��  �Q                               , �                          
                               �      ��                            ����                                                        �   l       ��                  5  <  �               �3u                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                 B  W  �               �bu                    O   ����    e�          O   ����    R�          O   ����    ��      �   �   I     @  �   J           P      $  �	      �  �      ��                  L  U                ��u                       L  �       |  �       ��                            7   ����	          ��               �e    �                              6   L  	      p   ��         @  �e    �                                                                    Le   Xe   de   pe   |e   �e   �e                 �  �           �e  �e  �e           �e  �e  �e              	        �  	 �        O   ����  e�          O   ����  R�          O   ����  ��      4  9   S         �   T        T  ~       
  L L L      O O O      0 0 0      * * *              
 
 
      g g g                                      D D D      1 1 1              B B B              ! ! !      = = =              C C C      ' ' '      " " "      i i i      h h h      k k k      j j j      t t t              c c c      4 4 4      @ @ @      H H H      % % %      ; ; ;              a a a      ] ] ]              - - -      . . .      ) ) )      G G G      : : :              E E E      P P P              / / /      5 5 5      + + +      , , ,      3 3 3      & & &      f f f                      K K K              e e e              I I I              2 2 2      # # #      d d d              J J J      $ $ $      T T T      U U U      V V V      W W W      X X X      Y Y Y      Z Z Z      [ [ [      \ \ \      ^ ^ ^      _ _ _      ` ` `      l l l              F F F      v v v      y y y      | | |      R R R              ? ? ?      M M M              > > >                      ( ( (      < < <      u u u      z z z      7 7 7              Q Q Q      	 	 	              S S S   
  N N N                 6 6 6      x x x      { { {      p p p      q q q      } } }      s s s      o o o      r r r      ~ ~ ~      n n n      m m m      w w w                      8 8 8      b b b      9 9 9      A A A             ��                             ��                            ����                                =   U                     �           �   l       ��                  ]  j  �               �s                    O   ����    e�          O   ����    R�          O   ����    ��           g  �   �       �f      4   �����f      n   h     �          �f        i    ,      �f      4   �����f      �   i  g    ��                            ����                                            $          �   l       ��                  p  �  �               ��u                    O   ����    e�          O   ����    R�          O   ����    ��       g  �           ,g  �          8g  �          Dg  �          Pg  �          \g  �              � ߱        �  Z   z  �    �        g                  �               �              �              �              �              �              � 	             � 
             �              � ߱          h   }  P   �        hg              �  s   �  H                 �              t  �       ��                            7   ����           ��                     �                              6   �         8   ��                    �                                                                            �  t                                   @            T   d          tg  �g  �g  �g          �  T  s   �  �                P                `       ��                            7   ����           ��                     �            �                  6   �         �   ��                    �            �                                                                                                     @            �              �g  �g  �g  �g          0      
   �  �� p             �g    ��                              ��                          ����                            �        2                 �    �       2                 �                    �           �   l       ��                  �  �  �               �qt                    O   ����    e�          O   ����    R�          O   ����    ��      �     �  �   �       �g      4   �����g      O   �  ��  ��  �g    9   �       �   �        (  ~       
  L L L      O O O      0 0 0      * * *              
 
 
      g g g                                      D D D      1 1 1              B B B              ! ! !      = = =              C C C      ' ' '      " " "      i i i      h h h      k k k      j j j      t t t              c c c      4 4 4      @ @ @      H H H      % % %      ; ; ;              a a a      ] ] ]              - - -      . . .      ) ) )      G G G      : : :              E E E      P P P              / / /      5 5 5      + + +      , , ,      3 3 3      & & &      f f f                      K K K              e e e              I I I              2 2 2      # # #      d d d              J J J      $ $ $      T T T      U U U      V V V      W W W      X X X      Y Y Y      Z Z Z      [ [ [      \ \ \      ^ ^ ^      _ _ _      ` ` `      l l l              F F F      v v v      y y y      | | |      R R R              ? ? ?      M M M              > > >                      ( ( (      < < <      u u u      z z z      7 7 7              Q Q Q      	 	 	              S S S   
  N N N                 6 6 6      x x x      { { {      p p p      q q q      } } }      s s s      o o o      r r r      ~ ~ ~      n n n      m m m      w w w                      8 8 8      b b b      9 9 9      A A A           (  :   �             �  s   �  T                 �              �  �       ��                            7   ����           ��                     �                               6   �         D   ��                    �                                                                             �  �                                   @            `   p           h  h  h  $h          �      s   �  �                \
              	  l	       ��                            7   ����           ��                     �            �	                  6   �         �	   ��                    �            �	                                                                (
  
                                   @            �	   
          0h  <h  Hh  Th          <
    ��                            ����                            �        2                 �    �       2                 �                    �           �   l       ��                  �  �  �               x!u                    O   ����    e�          O   ����    R�          O   ����    ��      �     �  `h  }          O   �  ��  ��  th    ��                            ����                                                       �   l       ��                 �  �  �               x�t                    O   ����    e�          O   ����    R�          O   ����    ��            0      �          �  �      ��                  �  �  �              Lt                0     �  �       \  �       ��                            7   ����           ��                     �            �                  6   �         ,   ��                  �            �                                                        �h                 t  h           �h           �h                      H   X        O   ����  e�          O   ����  R�          O   ����  ��      �    �  �  �  T  �h      4   �����h      $  �  (  ���                       �h                         � ߱            $  �  �  ���                       �h                         � ߱              �  �  �      Ti      4   ����Ti      $  �    ���                       ti                         � ߱        D  �   �  �i      �  $  �  p  ���                       �i                         � ߱        �  /   �  �                                 3   �����i      /   �                                  3   �����i               h          X  `    H                                             ��                             ��                              ��                          ����                                            �           �   l       ��                  �  �  �               �<t                    O   ����    e�          O   ����    R�          O   ����    ��      �     �  �   �       j      4   ����j      O   �  ��  ��  j    9   �       �   �        (  ~       
  L L L      O O O      0 0 0      * * *              
 
 
      g g g                                      D D D      1 1 1              B B B              ! ! !      = = =              C C C      ' ' '      " " "      i i i      h h h      k k k      j j j      t t t              c c c      4 4 4      @ @ @      H H H      % % %      ; ; ;              a a a      ] ] ]              - - -      . . .      ) ) )      G G G      : : :              E E E      P P P              / / /      5 5 5      + + +      , , ,      3 3 3      & & &      f f f                      K K K              e e e              I I I              2 2 2      # # #      d d d              J J J      $ $ $      T T T      U U U      V V V      W W W      X X X      Y Y Y      Z Z Z      [ [ [      \ \ \      ^ ^ ^      _ _ _      ` ` `      l l l              F F F      v v v      y y y      | | |      R R R              ? ? ?      M M M              > > >                      ( ( (      < < <      u u u      z z z      7 7 7              Q Q Q      	 	 	              S S S   
  N N N                 6 6 6      x x x      { { {      p p p      q q q      } } }      s s s      o o o      r r r      ~ ~ ~      n n n      m m m      w w w                      8 8 8      b b b      9 9 9      A A A           (  :   �             �  s   �  T                 �              �  �       ��                            7   ����           ��                     �                               6   �         D   ��                    �                                                                             �  �                                   @            `   p          (j  4j  @j  Lj          �      s   �  �                \
              	  l	       ��                            7   ����           ��                     �            �	                  6   �         �	   ��                    �            �	                                                                (
  
                                   @            �	   
          Xj  dj  pj  |j          <
    ��                            ����                            �        2                 �    �       2                 �      . �          �  (   ��  �  �                      
 �                                                                   )      �       W&                                    
 �                                                                  0      �  
     �^&                                    
 �                                                                  ,     �  
     �f&                                    
 �                                                                    �     �         r&                                    
 �                                                                  �      �       �v&                                    
 �                                                                  �      �         �&                                      �                                                                                                                                                                  2 D          �  p   ��  �  �                      
 �                                                                    )      �       �W&                                    
 �                                                                   0      �  
     �^&                                    
 �                                                                   ,     �  
     �f&                                    
 �                                                                    �     �         r&                                    
 �                                                                   �      �       �v&                                    
 �                                                                   �      �       0�&                                      �                                                                                                                                                           �   d d     0   ��K5�L5  � �                                                                                                                       d     D                                                                 P   @w fQ                                                           �&  G     p  @w �X                                                         J                                 P   @� Q                                                           �&  G   
 X  @� �Q                                                        |          
 X  �
� 0Q                                                        �       2    P   @8�Q                                                           �&  G   
 X  @8�Q                                                        m          
 X  �
80Q                                                        ]           `   8B !                                                       =        $         B !      \  )^��                                 4                  �&      �        @      `  �.^B !                                                       �        $         B !      \  �.^��             �                   �          "       �&      d        H      t  @�lM                                                        �     -  
               �   %  �    H  ,D                                �          �           H  �	                                �          ,          `  �4K <                                                               $         K <      \  ��L� 
                                �                 �&      $        @      `  ��K <                                                       !        $         K <      \  ��L�                                                  �&      �        @      P ����2                                                        8        D                                                                    TXS appSrvUtils T-CDocu-01 CodCia CodDoc NroDoc FchDoc CodCli NomCli DirCli RucCli CodAnt CodPed NroPed NroOrd ImpBrt ImpExo PorIgv ImpIgv ImpDto ImpTot SdoAct FlgEst usuario UsrDscto FlgCie FchCie HorCie CodMon TpoCmb CodAlm LugEnt Tipo CodMov CodVen ImpIsc ImpVta ImpFle FchCan Glosa CodRef NroRef FchVto CodCob CodCta CodAge FlgUbi FlgUbiA FchUbi FchUbiA FlgSit Cndcre CodDiv ImpInt FmaPgo FchAct FlgSitA TipVta PorDto TpoFac FchCbd NroSal FlgCbd Codope NroMes Nroast FchAnu UsuAnu CodDpto CodProv CodDist FlgCon LugEnt2 FlgAte FchAte imptot2 ImpCto AcuBon NroCard TipBon CCo FlgEnv puntos mrguti Sede Libre_c01 Libre_c02 Libre_c03 Libre_c04 Libre_c05 Libre_d01 Libre_d02 Libre_f01 Libre_f02 FchCre Libre_f03 Libre_f04 Libre_f05 FchCobranza UsrCobranza DivOri ImpPro ImpDto2 GlosaImpDto2 CodCaja Dcto_Otros_Mot Dcto_Otros_Factor Dcto_Otros_VV Dcto_Otros_PV Lista_de_Precios TotalValorVentaNetoOpGravadas TotalValorVentaNetoOpGratuitas TotalTributosOpeGratuitas TotalIGV TotalImpuestos TotalValorVenta TotalPrecioVenta DescuentosGlobales PorcentajeDsctoGlobal MontoBaseDescuentoGlobal TotalValorVentaNetoOpNoGravada TotalDocumentoAnticipo MontoBaseDsctoGlobalAnticipo PorcentajeDsctoGlobalAnticipo TotalDsctoGlobalesAnticipo MontoBaseICBPER TotalMontoICBPER TotalValorVentaNetoOpExoneradas TotalVenta T-CDocu-02 ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST c-Mon S/. US$ s-codcia cb-codcia s-coddiv x-coddiv x-codmon wWin BtnDone img/b-cancel.bmp BUTTON-1 img/arrow-right.jpg BUTTON-2 img/arrow-left.jpg BUTTON-3 img/b-ok.bmp COMBO-BOX-Division FILL-IN-Agencia FILL-IN-CodAge FILL-IN-CodCta FILL-IN_Banco RADIO-SET-CodMon BROWSE-3 LETRAS EN CARTERA X(12) 99/99/9999 x(3) ->>>,>>9.99 BROWSE-4 LETRAS PARA ENVIAR AL BANCO fMain X(256) x(50) SOLES DOLARES ->,>>>,>>9 Moneda: GUI ENVIO DE LETRAS AL BANCO - EN TRANSITO DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   COMBO-BOX-Division FILL-IN-CodCta FILL-IN-CodAge BUTTON-3 BtnDone BROWSE-3 BROWSE-4 BUTTON-1 BUTTON-2 CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE  Debe ingresar primero el banco ENTRY  -  gn-agbco Agencias por Banco cb-ctas Plan de Cuentas Contable Cuenta contable no existe 9 iStartPage ADM-ERROR ADM-CREATE-OBJECTS CcbCDocu LET P C CARGA-TEMPORALES DISABLE_UI ENABLE_UI ENVIAR EXITOBJECT cLista GN-DIVI DIVISIONES , INITIALIZEOBJECT REGRESAR llave00 llave01 llave02 llave03 llave04 llave05 llave06 llave07 llave08 llave09 llave10 llave11 llave12 llave13 llave14 Llave15 Llave16 Numero Emisi�n Vencimiento Mon Importe Total Saldo Actual Divisi�n Banco Agencia Button 3 &Done Button 1 Button 2 Llave01 idx00 IDX01 �  X9  $   @      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   �	  �	  �	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc                8  E  G  H  �     9                                     �  	     :                                       �  L	     ;                                       	  �	     <                                   Z  T	  �	     =                                   i  j  �	  �	     >                                   u  v  w  �	  ,
     ?                                   �  �  �	  d
     @                                   �  �  �  �  �  �  �  4
  �
     A                                   �  �  �  �  �  �  �  �
  �
     B                                   �  �  �  �  �  �
  @     C                                   �  �  �  �    �     D                                   �  �  �  �  �  �  �  �  �  �  �  �  �  �  P  �     E                                       	  
  �  (     F                                       �  t     G               `                  adm-create-objects  <  0  �     H               �                  Carga-Temporales    I  J  L  S  T  U  W  x       I                                 disable_UI  g  h  i  j  �  `     J               T                  enable_UI   z  }  �  �  �  $  �     K               �                  Enviar  �  �  �  �  �  �  �  t       L               �                  exitObject  �  �  �            $     cLista  �  p     M             \                  initializeObject    �  �  �  �  �  �  �  �  �  �  �  ,  �     N               �                  Regresar    �  �  �  �  �  �  �  �  �,  
     L&      T,                      �  4  @  ~   T-CDocu-01  (         0         8         @         H         P         X         `         h         p         x         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                                        (         0         8         @         H         P         X         `         h         p         x         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                             $         ,         4         <         D         L         T         \         d         l         t         |  
      �         �  
      �         �         �         �         �         �         �         �         �         �         �                                     $         ,         8         D         P         \         h         p         x         �         �         �         �         �         �         �         �                  0         L         X         h         x         �         �         �         �         �                  ,         L         h         x         �         �         CodCia  CodDoc  NroDoc  FchDoc  CodCli  NomCli  DirCli  RucCli  CodAnt  CodPed  NroPed  NroOrd  ImpBrt  ImpExo  PorIgv  ImpIgv  ImpDto  ImpTot  SdoAct  FlgEst  CodCob  CodCta  usuario FlgCie  FchCie  HorCie  CodMon  TpoCmb  CodAlm  LugEnt  Tipo    CodMov  CodVen  ImpIsc  ImpVta  FchCan  Glosa   CodRef  NroRef  FchVto  CodAge  FlgUbi  FlgUbiA FchUbi  FchUbiA FlgSit  Cndcre  CodDiv  ImpInt  FmaPgo  FchAct  FlgSitA TipVta  PorDto  TpoFac  UsrDscto    FlgCbd  FchCbd  NroSal  Codope  NroMes  Nroast  FchAnu  UsuAnu  CodDpto CodProv CodDist FlgCon  LugEnt2 FlgAte  FchAte  ImpFle  imptot2 ImpCto  AcuBon  NroCard TipBon  CCo FlgEnv  puntos  mrguti  Sede    Libre_c01   Libre_c02   Libre_c03   Libre_c04   Libre_c05   Libre_d01   Libre_d02   Libre_f01   Libre_f02   FchCre  Libre_f03   Libre_f04   Libre_f05   FchCobranza UsrCobranza DivOri  ImpPro  ImpDto2 GlosaImpDto2    CodCaja Dcto_Otros_Mot  Dcto_Otros_Factor   Dcto_Otros_VV   Dcto_Otros_PV   Lista_de_Precios    TotalValorVentaNetoOpGravadas   TotalValorVentaNetoOpGratuitas  TotalTributosOpeGratuitas   TotalIGV    TotalImpuestos  TotalValorVenta TotalPrecioVenta    DescuentosGlobales  PorcentajeDsctoGlobal   MontoBaseDescuentoGlobal    TotalValorVentaNetoOpNoGravada  TotalDocumentoAnticipo  MontoBaseDsctoGlobalAnticipo    PorcentajeDsctoGlobalAnticipo   TotalDsctoGlobalesAnticipo  MontoBaseICBPER TotalMontoICBPER    TotalValorVentaNetoOpExoneradas TotalVenta      �  �  ~   T-CDocu-02  �          �          �          �          �          �          �          �          �          !         !         !         !         $!         ,!         4!         <!         D!         L!         T!         \!         d!         l!         t!         |!         �!         �!         �!         �!         �!         �!         �!         �!         �!         �!         �!         �!         �!         �!         �!         �!         "         "         "         "         $"         ,"         4"         <"         D"         L"         T"         \"         d"         l"         t"         �"         �"         �"         �"         �"         �"         �"         �"         �"         �"         �"         �"         �"         �"         �"         �"          #         #         #  
      #          #  
      (#         ,#         4#         <#         D#         L#         X#         d#         p#         |#         �#         �#         �#         �#         �#         �#         �#         �#         �#         �#         �#         $         $         $         $$         ,$         <$         P$         `$         p$         �$         �$         �$         �$         �$         �$         %          %         4%         L%         h%         �%         �%         �%         �%         �%         &          &         @&         CodCia  CodDoc  NroDoc  FchDoc  CodCli  NomCli  DirCli  RucCli  CodAnt  CodPed  NroPed  NroOrd  ImpBrt  ImpExo  PorIgv  ImpIgv  ImpDto  ImpTot  SdoAct  FlgEst  CodCob  CodCta  usuario FlgCie  FchCie  HorCie  CodMon  TpoCmb  CodAlm  LugEnt  Tipo    CodMov  CodVen  ImpIsc  ImpVta  FchCan  Glosa   CodRef  NroRef  FchVto  CodAge  FlgUbi  FlgUbiA FchUbi  FchUbiA FlgSit  Cndcre  CodDiv  ImpInt  FmaPgo  FchAct  FlgSitA TipVta  PorDto  TpoFac  UsrDscto    FlgCbd  FchCbd  NroSal  Codope  NroMes  Nroast  FchAnu  UsuAnu  CodDpto CodProv CodDist FlgCon  LugEnt2 FlgAte  FchAte  ImpFle  imptot2 ImpCto  AcuBon  NroCard TipBon  CCo FlgEnv  puntos  mrguti  Sede    Libre_c01   Libre_c02   Libre_c03   Libre_c04   Libre_c05   Libre_d01   Libre_d02   Libre_f01   Libre_f02   FchCre  Libre_f03   Libre_f04   Libre_f05   FchCobranza UsrCobranza DivOri  ImpPro  ImpDto2 GlosaImpDto2    CodCaja Dcto_Otros_Mot  Dcto_Otros_Factor   Dcto_Otros_VV   Dcto_Otros_PV   Lista_de_Precios    TotalValorVentaNetoOpGravadas   TotalValorVentaNetoOpGratuitas  TotalTributosOpeGratuitas   TotalIGV    TotalImpuestos  TotalValorVenta TotalPrecioVenta    DescuentosGlobales  PorcentajeDsctoGlobal   MontoBaseDescuentoGlobal    TotalValorVentaNetoOpNoGravada  TotalDocumentoAnticipo  MontoBaseDsctoGlobalAnticipo    PorcentajeDsctoGlobalAnticipo   TotalDsctoGlobalesAnticipo  MontoBaseICBPER TotalMontoICBPER    TotalValorVentaNetoOpExoneradas TotalVenta  l&          `&  
   appSrvUtils �&      �&     c-Mon   �&        �&     s-codcia    �&        �&     cb-codcia   �&        �&     s-coddiv    '       �&     x-coddiv    ('       '     x-codmon    D'       <'  
   wWin    l'       X'     COMBO-BOX-Division  �'       �'     FILL-IN-Agencia �'       �'     FILL-IN-CodAge  �'       �'     FILL-IN-CodCta  �'       �'     FILL-IN_Banco   $(    	   (     RADIO-SET-CodMon    L(        8(  
   gshAstraAppserver   t(  	 	     `(  
   gshSessionManager   �(  
 
     �(  
   gshRIManager    �(        �(  
   gshSecurityManager  �(        �(  
   gshProfileManager   )        �(  
   gshRepositoryManager    @)        ()  
   gshTranslationManager   d)        T)  
   gshWebManager   �)        x)     gscSessionId    �)        �)     gsdSessionObj   �)        �)  
   gshFinManager   �)        �)  
   gshGenManager   *        *  
   gshAgnManager   <*        ,*     gsdTempUniqueID \*        P*     gsdUserObj  �*        p*     gsdRenderTypeObj    �*        �*     gsdSessionScopeObj  �*    
   �*  
   ghProp  �*       �*  
   ghADMProps  +       �*  
   ghADMPropsBuf   4+        +     glADMLoadFromRepos  P+       H+     glADMOk p+       d+  
   ghContainer �+       �+     cObjectName �+       �+     iStart  �+       �+     cAppService �+       �+     cASDivision ,        ,     cServerOperatingMode    4,       ,,     cFields          H,     iStartPage  p,    L  d,  T-CDocu-01  �,    L  �,  T-CDocu-02  �,       �,  gn-agbco    �,       �,  cb-ctas �,       �,  CcbCDocu              �,  GN-DIVI          B   �   �  �  �  
              �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  	  	  	  	  	  	  		  
	  	  	  	  	  	  	  	  	  �	  �	  �	   
  
  
  
  
  
  
  
  
  	
  

  
  
  
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
    )  *  -  .  /  0  2  3  5  6  7  8  9  :  ;  <  >  ?  @  A  B  C  E  F  G  H  I  J  K  L  M  N  O  P  Q  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _  `  a  b  c  d  e  f  g  h  i  j  �  �     !  *  +  /  0  1  3  6  @  \  n  �  �  �  �  N  f  g  �  �  �  �  �  �  �  �  �  �  �    �  �  �  �              3  4  5  7  ?  E  K  N  S  W  X  Y  [  _  �  �  +  ,  d  q    �  �  �  �  �  �  �            #  $  %  &  (  *      H� $ C:\Progress\OpenEdge\src\adm2\windowmn.i 1  f!  C:\Progress\OpenEdge\src\adm2\containr.i P1  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �1  ��  C:\Progress\OpenEdge\src\adm2\visual.i   �1  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �1  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    <2  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   t2  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �2  Ds   C:\Progress\OpenEdge\gui\fn  �2  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   3  Q.  C:\Progress\OpenEdge\gui\set T3  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i |3  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �3  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �3  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  84  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i l4  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �4  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i �4  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i     5  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    d5  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �5  �j  C:\Progress\OpenEdge\gui\get �5  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    6  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    H6  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �6  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �6  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �6  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   47  �  C:\Progress\OpenEdge\src\adm2\appsprto.i x7  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �7  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  �7  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  (8  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i l8  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �8  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �8  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i    9  r�   d:\newsie\on_in_co\APLIC\ccb\wenvletbco-01.w       -      �9     �  $   �9  +        �9  �        �9     �     �9  �   �     �9     �     �9  �   �      :     d  #   :  �   N      :     L      0:  �   E     @:     C      P:  �   B     `:     @      p:  r   $     �:  n        �:     �  "   �:  i   �     �:     �     �:  P   t     �:  �   k     �:       !   �:  �         ;     �     ;  �   �      ;     �     0;  �   �     @;     �     P;  g   �     `;     l     p;  O   T     �;  �   �     �;     �      �;  �   �     �;     T     �;  �   I     �;     '     �;  �   &     �;           <  �        <     �      <  �   �     0<     �     @<  �   �     P<     �     `<  �   �     p<     f     �<  }   Z     �<     8     �<     �     �<     n     �<          �<  7   �     �<  �   �     �<  O   �      =     �     =     n      =  �   &     0=  �        @=  O        P=     �
     `=     �
     p=  �   �
     �=  x   �
  
   �=  M   n
     �=     ]
     �=     
     �=  a   �	  
   �=  �  �	     �=     �	     �=  �  �	      >  O   y	     >     h	      >     	     0>  �   D     @>          P>     k     `>  x   e     p>     L     �>     �     �>     �     �>     �     �>     �     �>  Q   �  
   �>     8     �>       
   �>     �      ?     �  
   ?  f   �      ?     H  	   0?  "        @?     �     P?     �     `?  Z   ~     p?     �     �?     G     �?     3     �?          �?     �     �?  2   �       �?     K      �?     !       �?           