	��V�!b�5  E�              [                                "� 3584010Futf-8 MAIN O:\on_in_co\aplic\CCB\w-aprobacion-nc-otros.w,, PROCEDURE validar,,INPUT pCodDoc CHARACTER,INPUT pNroDoc CHARACTER,OUTPUT pRetVal CHARACTER PROCEDURE Rechazar-Rebate,, PROCEDURE initializeObject,, PROCEDURE Graba-Temp-FeLogErrores,, PROCEDURE Extorna-AC,, PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE Aprobar-Rebate,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER        �z              �j             �p �z   �              �              |;    +   l� �  7   � `  8   l� �   E   `� |&  F   � |  G   X   H   d" $  I   �# �   J   |$ l  K   �( �  L   �,   M   �9 p  N           (> ,  TC   ? hG >)  iSO8859-1                                                                           �y   ( �                                      �                  �                 z  0-    d-   ��   �  Dz         ��  �   �z      �z          H                                             PROGRESS                         H           
    
                    �              �                                                                                                     
           �          X  �5  2   6     �  �B�_�6                       �/          �0      �   l         �       �   X  �:  �   �;  �   �  �B�_4<  
       �              �6          �7      �   �             �                                                                                          �             d             P                                                                                          �  ��         �             �                                                                                          �             �         �       �  L  (t  �   �t  �  �  ���a�w  ~       �             \<          HF      �                INTEGRAL                         PROGRESS                         �  	   >  H      >                         ���a            >  �         �                  �                        �	  (  ~~     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2CODCAJADCTO_OTROS_MOTDCTO_OTROS_FACTORDCTO_OTROS_VVDCTO_OTROS_PVLISTA_DE_PRECIOSTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    x  
   G  H      G                         �B�_            P  ��                              �                        �     t      CODIGONOMBRECODCTAAFECTOCODCIATABLALIBRE_C01LIBRE_C02LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02RESERVADO                                                                      	          
                                                                      �  4      p  
    
                  \  $             �                                                                                          4          
  �  F        
    
                    �             �                                                                                          F          
  L  X      �  
    
                  �  |             8                                                                                          X          
  �  e      t  
    
                  `  (             �                                                                                          e          
  �  x         
    
                    �             �                                                                                          x          
  P  �      �  
    
                  �  �             <                                                                                          �          
  �  �      x  
    
                  d  ,             �                                                                                          �          
  �  �      $  
    
                    �             �                                                                                          �          
  T  �      �                         �  �             @                                                                                          �               �      |                        h  0             �                                                                                          �            �  �      (  
    
                    �             �                                                                                          �          
  X  �      �  
    
                  �  �             D                                                                                          �          
    �      �  
    
                  l  4             �                                                                                          �          
  �        ,                          �             �                                                                                                      \        �                        �  �             H                                                                                                        #      �                        p  8             �                                                                                          #                4      0                          �&             �                                                                                          4                                                                                                                                               	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                  :                  ;                  <                  =                  >                  ?                  @                  A                  B                  C                  D                  E                  F                  G                  H                  I                  J                  K              
   L                  M              
   N                  O                  P                  Q                  R                  S                  T                  U                  V                  W                  X                  Y                  Z                  [                  \                  ]                  ^                  _                  `                  a                  b                  c                  d                  e                  f                  g                  h                  i                  j                  k                  l                  m                  n                  o                  p                  q                  r                  s                  t                  u                  v                  w                  x                  y                  z                  {                  |                  }                  ~                                                   X(  #   M'  H      M'                        �B�_            M'  	�                              �   '                      �'  0'  q      CODCIACODDIVCODDOCNRODOCLOGDATELOGUSERCODHASHNUMDOCUMENTOFLAGPPLLESTADOPPLLID_POSIP_EPOSERRORDATELOGESTADOCAMPO-C                                              "                           	          
                                           "                  
            $   r'  H      r'                         �B�_            z'  ~                              �  �(                      �*  �(  �3     CODDIVDESDIVCODCIADIRDIVTELDIVFAXDIVRESPONFLGREPFLGPREUNIFLGAPRCOTFLGAPRPEDFLGPREVTADIASVTOCOTDIASVTOPEDDIASVTOO_DDIASAMPCOTTIPDTOMODPREUNIFLGEMPAQUEFLGROTACIONCANALVENTAFLGPICKINGFLGBARRASFLGMINVENTAFLGDTOPROMFLGDTOVOLFLGTARJETAVENTAMOSTRADORVENTAMAYORISTAVENTAMINORISTAFLGDTOCLFCLIPORDTOCLFCLIFLGDTOCNDVTALIBRE_C01LIBRE_C02LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02FLAGTIPOFLAGUSUARIOFLAGMIGRACIONFLAGFECHACAMPO-LOGCAMPO-DECCAMPO-DATECAMPO-CHARGRUPO_DIVI_GGCENTRO_COSTO                                                                       	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          .          /          0 
        1 
        2 
        3 
        4          5                        `�                                              + h�          �.  X/  \ ��-                                                    
    999-999999                 
             
                OTROS                   
             
             
                                         
                                                                                                                \   l   |   �   �   �   �   �   �   �           0  @  P  `  p  �  �  �  �      \   l   |   �   �   �   �   �   �   �          0  @  P  `  p  �  �  �  �                    "                                                                        "                                                                             	                  
                                                                    
                                    �3  �3  �3  �3  �3          �3             �3  �3  4  4                             4  4  $4  44  ,4                         84  @4  H4  X4  P4                         \4  d4  l4  |4  t4                         �4  �4  �4  �4                              �4  �4  �4  �4                              �4  �4  �4  �4                              �4  �4  �4  5                              5  5   5  05                              45  @5  L5  X5                              \5  h5  t5  �5                              �5  �5  �5  �5                              �5  �5  �5  �5                              �5  �5  �5  �5                                                                          CodCia  999 Cia Cia 0   C�digo de compa�ia  ErrorDate   99/99/9999 HH:MM:SS.SSS ErrorDate   ?   CodDiv  x(5)    C.Div   C.Div       CodDoc  x(3)    Codigo  Codigo      NroDoc  X(9)    Numero  Numero      LogDate 99/99/9999 HH:MM:SS.SSS LogDate ?   LogUser x(10)   LogUser     LogEstado   x(8)    LogEstado       CodHash x(20)   CodHash     NumDocumento    x(20)   NumDocumento        FlagPPLL    ->,>>>,>>9  FlagPPLL    0   EstadoPPLL  ->,>>>,>>9  EstadoPPLL  0   ID_Pos  x(15)   ID_Pos      IP_ePos x(25)   IP_ePos     Campo-C x(8)    Campo-C     �  �  ���������    �       � �           �      �'        �'        �'                �     i  i      i  i  i      i  i  i     	 "	 	 	 	       0   7   >   E   M   _   g   t   }   �   �   &   U   �                                                                                                                                  	                  
                                                  �9  �9  �9  �9  �9                         �9  �9  �9  �9                             �9  �9  �9  �9                              �9  �9  :  :                             :  :  ,:  4:                              8:  @:  H:  P:                              T:  \:  h:  p:                             t:  |:  �:  �:                             �:  �:  �:  �:                              �:  �:  �:  �:                                                                          Task-No 999999  Numero de Tarea Nro!Tarea   0   Llave-C x(8)    Llave-C     Campo-D 99/99/9999  Campo-D ?   Campo-F ->>>,>>>,>>9.9999   Campo-F 0   Campo-I ->>>,>>>,>>9    Campo-I 0   Campo-C X(8)    Campo-C     Llave-I >>>>>>>>>9  Llave-I 0   Llave-F >>>>>>>>>9  Llave-F 0   Llave-D 99/99/9999  Llave-D ?   Campo-L Si/No   Campo-L Si  �  ���������   � $�  �������������������������������� $�                                �� $�                                �� $�                                � �� B�  �     �'        �'        �'                �     i  i      i  i      i  i     	 	 		 	    �   �   �   �   �   �   �   �   �                                                                                                                                        	                  
                                                                                                                                                                                                                                          9                                                                                                                                                                   !                  "                  #                  $                  I                  %                  &                  '                  (                  )                                                      *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  ;                  <                  :                  =                  >                  ?                  @                  A                  B                  C                  D                  E                  F                  G                  H                  J                  K              
   L                  M              
   N                  O                  P                  Q                  R                  S                  T                  U                  V                  W                  X                  Y                  Z                  [                  \                  ]                  ^                  _                  `                  a                  b                  c                  d                  e                  f                  g                  h                  i                  j                  k                  l                  m                  n                  o                  p                  q                  r                  s                  t                  u                  v                  w                  x                  y                  z                  {                  |                  }                  ~                                                   \  $\  (\  0\  ,\          4\             H\  P\  X\  h\  `\                         l\  t\  |\  �\  �\                         �\  �\  �\  �\  �\                         �\  �\  �\  �\  �\                         �\  �\  �\   ]  �\                         ]  ]  ]  ,]   ]                          0]  8]  @]  H]  D]                          L]  T]  \]  |]  l]                          �]  �]  �]  �]                             �]  �]  �]  �]  �]                         �]  �]  �]  �]  �]                          �]  �]  ^  ,^  ^                         0^  8^  H^  p^  \^                         t^  |^  �^  �^  �^                         �^  �^  �^  �^  �^                         �^  �^  �^  _  _                          _  (_  8_  X_  H_                         \_  d_  t_  �_  �_                         �_  �_  �_  �_  �_                         �_  �_  �_  �_  �_                          �_  �_  �_  `   `                          `  `   `  (`                              ,`  4`  @`  P`  H`                          T`  \`  d`  �`  t`                          �`  �`  �`  �`  �`                          �`  �`  �`  �`  �`                         �`  �`  �`  �`  �`                          a  a  a  8a  $a                          <a  Da  La  ta  `a                          xa  �a  �a  �a  �a          �a              �a  �a  �a  �a  �a                          �a  �a  b  $b  b                         (b  0b  @b  Xb  Lb                         \b  db  tb  �b  �b                         �b  �b  �b  �b  �b                          �b  �b  �b  c   c                          c  c  $c  4c  ,c                         8c  @c  Hc  Xc  Pc                         \c  dc  pc  �c  �c                         �c  �c  �c  �c  �c                          �c  �c  �c  �c  �c                         �c  d  d  d  d                           d  (d  ,d  Dd  8d                          Hd  Pd  Td  ld  `d                          pd  xd  �d  �d  �d                          �d  �d  �d  �d  �d                          �d  �d  �d  �d  �d                           e  e  e  0e  $e                          4e  <e  De  Te  Le                         \e  de  te  �e  �e                         �e  �e  �e  �e  �e                          �e  �e  �e  �e                              �e  �e  �e  f                              f  f   f  8f  ,f                          <f  Df  Lf  df  Xf                         hf  pf  xf  �f  �f                         �f  �f  �f  �f  �f                          �f  �f  �f  �f  �f                          �f  �f  �f   g                              g  g  g   g  g          $g              Dg  Lg  Pg  Xg  Tg          \g              xg  �g  �g  �g  �g          �g              �g  �g  �g  �g  �g                          �g  �g  �g  h  h                          h  h   h  @h  0h                          Dh  Lh  Th  lh  `h                          ph  xh  �h  �h  �h                          �h  �h  �h  �h  �h                         �h  �h  �h  i  �h                          i  i  i  (i   i                         ,i  4i  @i  Pi  Hi          Ti              ti  |i  �i  �i                             �i  �i  �i  �i  �i                         �i  �i  �i  �i  �i                         �i  �i   j  j  j                          j  j   j  0j  (j                          4j  8j  @j  `j  Pj          dj              tj  |j  �j  �j                              �j  �j  �j  �j                             �j  �j  �j  �j                             �j  �j  �j  �j                               k  k  k   k                              $k  0k  8k  Dk                              Hk  Tk  \k  hk                              lk  xk  �k  �k                              �k  �k  �k  �k                              �k  �k  �k  �k                             �k  �k  l  l                             l   l  ,l  8l                              <l  Hl  Tl  `l                              dl  ll  xl  �l                              �l  �l  �l  �l                              �l  �l  �l  �l                              �l  �l  �l  �l                              �l  m  m  $m                              (m  4m  <m  Pm                              Tm  \m  dm  tm  lm                         xm  �m  �m  �m                             �m  �m  �m  �m                             �m  �m  �m  �m                              �m  �m  �m  n  n                          n  (n  0n  @n                              Dn  Xn  hn  |n                             �n  �n  �n  �n                             �n  �n  �n  �n                             �n  o  o   o                              $o  Do  To  to                             xo  �o  �o  �o                             �o  �o  �o  p                             p  $p  4p  @p                             Dp  Tp  dp  tp                             xp  �p  �p  �p                             �p  �p  �p  �p                             �p  �p  q   q                             $q  <q  Hq  `q                             dq  �q  �q  �q                             �q  �q  �q   r                             r  r  ,r  Dr                             Hr  hr  xr  �r                             �r  �r  �r  �r                             �r  s  s  4s                             8s  Hs  Xs  hs                             ls  �s  �s  �s                             �s  �s  �s  �s                             �s  t  t  $t                                                                         CodCia  999 Cia Cia 0   C�digo de compa�ia  CodDoc  x(3)    Codigo  Codigo      NroDoc  X(12)   Numero  Numero      FchDoc  99/99/9999  Fecha   Fecha   TODAY   CodCli  x(11)   Cliente Cliente     NomCli  x(50)   Nombre  Nombre      DirCli  x(60)   Direccion   Direccion       RucCli  x(11)   Ruc Ruc     CodAnt  X(10)   Codigo Anterior Codigo!Anterior     CodPed  x(10)   CodPed      NroPed  X(12)   Pedido  Pedido      NroOrd  x(12)   Orden de Compra Orden de!Compra     ImpBrt  ->>,>>>,>>9.99  Importe Bruto   Importe Bruto   0   ImpExo  ->>,>>>,>>9.99  Importe Exonerado   Importe Exonerado   0   PorIgv  ->>9.99 % I.G.V.    % I.G.V.    0   ImpIgv  ->>,>>>,>>9.99  Importe I.G.V.  Importe I.G.V.  0   ImpDto  ->>,>>>,>>9.99  Importe Descuento   Importe Descuento   0   ImpTot  ->>,>>>,>>9.99  Importe Total   Importe Total   0   SdoAct  ->>,>>>,>>9.99  Importe Total   Importe Total   0   FlgEst  X   Estado  Estado  P   usuario x(10)   usuario usuario     UsrDscto    X(10)   Resp.!Dscto.    Resp.!Dscto.        FlgCie  x   FlgCie  P   FchCie  99/99/9999  Cierre  Cierre  ?   HorCie  x(5)    Hora de cierre  Hora de!cierre      CodMon  9   Moneda  Moneda  1   TpoCmb  Z,ZZ9.9999  Tipo de cambio  T/Cambio    0   CodAlm  x(3)    Almacen Almacen     LugEnt  x(60)   Lugar de entrega    Lugar de entrega        Tipo    x(20)   Tipo de documento   Tipo de documento       CodMov  99  C�digo de movimiento    C�digo!movimto. 0   C�digo de movimiento    CodVen  x(10)   Vendedor    Vendedor        ImpIsc  ->>,>>>,>>9.99  Importe Isc Importe Isc 0   ImpVta  ->>,>>>,>>9.99  Valor Venta Valor venta 0   ImpFle  ->>,>>>,>>9.99  Importe Flete   Importe Flete   0   FchCan  99/99/9999  Fecha de cancelacion    Fecha de!cancelacion    ?   Glosa   x(60)   Observaciones   Observaciones       CodRef  x(3)    Codigo  Codigo      NroRef  X(12)   Numero  Numero      FchVto  99/99/9999  Fecha de vencimiento    Fecha de!Vencimiento    ?   CodCob  X(10)   Cobrador    Cobrador        CodCta  X(10)   Cuenta Contable Cuenta      CodAge  X(10)   Agencia Agencia     FlgUbi  X   Ubicaci�n   Ubicaci�n       FlgUbiA X   Ubicaci�n   Ubicaci�n       FchUbi  99/99/9999  Fecha de Ubicaci�n  Fecha   ?   FchUbiA 99/99/9999  Fecha de Ubicaci�n  Fecha   ?   FlgSit  X   Situaci�n   Situaci�n       Cndcre  X   Condicion de Credito    Condicion       CodDiv  x(5)    C.Div   C.Div   00000   ImpInt  ->>,>>>,>>9.99  Intereses   Intereses   0   FmaPgo  X(8)    Condicion de ventas Condicion de!venta      FchAct  99/99/9999  FchAct  ?   FlgSitA X   Situacion Anterior      TipVta  X(1)    Tipo Venta  Tipo venta      PorDto  >>9.99  % Dscto.    % Dscto.    0   TpoFac  X(1)    Tipo    Tipo        FchCbd  99/99/9999  Fecha   Fecha   ?   NroSal  X(12)   Numero Salida   Numero!Salida       FlgCbd  yes/no  FlgCbd  no  Codope  xxx Operacion   Ope     Ingrese la Operacion Contable   NroMes  99  Mes Mes 0   Ingrese el mes de trabajo   Nroast  x(6)    Asiento Comprobte       Ingrese el Nro. de Asiento  FchAnu  99/99/99    Fecha   Fecha!Anulacion ?   UsuAnu  X(10)   Usuario Usuario     CodDpto X(2)    Departamento    Departamento        CodProv X(2)    Provincia   Provincia       CodDist X(2)    Distrito    Distrito        FlgCon  x(1)    Flag Control    Flag!Control        LugEnt2 x(60)   Lugar de entrega    Lugar de entrega        FlgAte  X   Estado  Estado      FchAte  99/99/9999  Fecha   Fecha   ?   Fecha de Despacho de Almacen    imptot2 ->>>,>>>,>>9.99 imptot2 0   ImpCto  ->>,>>>,>>9.99  ImpCto  ImpCto  0   AcuBon  ->>>,>>>,>>9.99 AcuBon  AcuBon  0   NroCard x(8)    NroCard Nrocard     TipBon  99  TipBon  TipBon  0   CCo X(5)    Centro de Costo Centro!de Costo     Centro de Costo FlgEnv  Si/No   El pedido es para enviar?   No  puntos  ->>,>>9.99  puntos  0   mrguti  ->>,>>9.99  mrguti  0   Sede    x(5)    Sede        Libre_c01   x(60)   Libre_c01       Libre_c02   x(60)   Libre_c02       Libre_c03   x(60)   Libre_c03       Libre_c04   x(60)   Libre_c04       Libre_c05   x(60)   Libre_c05       Libre_d01   ->>>,>>>,>>9.99<<<  Libre_d01   0   Libre_d02   ->>>,>>>,>>9.99<<<  Libre_d02   0   Libre_f01   99/99/99    Libre_f01   ?   Libre_f02   99/99/99    Libre_f02   ?   FchCre  99/99/99    FchCre  ?   Libre_f03   99/99/99    Libre_f03   ?   Libre_f04   99/99/99    Libre_f04   ?   Libre_f05   99/99/99    Libre_f05   ?   FchCobranza 99/99/9999  Fecha Cobranza  ?   UsrCobranza x(10)   Usuario Cobranza        DivOri  x(5)    Origen  Origen      ImpPro  ->>>,>>>,>>9.99 ImpPro  0   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   GlosaImpDto2    x(30)   GlosaImpDto2        CodCaja X(10)   Codigo Caja Codigo!Caja     Dcto_Otros_Mot  x(20)   Dcto_Otros_Mot      Dcto_Otros_Factor   ->>,>>9.999999  Dcto_Otros_Factor   0   Dcto_Otros_VV   ->>>,>>>,>>9.999999 Dcto_Otros_VV   0   Dcto_Otros_PV   ->>>,>>>,>>9.999999 Dcto_Otros_PV   0   Lista_de_Precios    x(8)    Lista_de_Precios        TotalValorVentaNetoOpGravadas   >>>>>>>>>>>9.99 TotalValorVentaNetoOpGravadas   0   TotalValorVentaNetoOpGratuitas  >>>>>>>>>>>9.99 TotalValorVentaNetoOpGratuitas  0   TotalTributosOpeGratuitas   >>>>>>>>>>>9.99 TotalTributosOpeGratuitas   0   TotalIGV    >>>>>>>>>>>9.99 TotalIGV    0   TotalImpuestos  >>>>>>>>>>>9.99 TotalImpuestos  0   TotalValorVenta >>>>>>>>>>>9.99 TotalValorVenta 0   TotalPrecioVenta    >>>>>>>>>>>9.99 TotalPrecioVenta    0   DescuentosGlobales  >>>>>>>>>>>9.99 DescuentosGlobales  0   PorcentajeDsctoGlobal   >>9.99999   PorcentajeDsctoGlobal   0   MontoBaseDescuentoGlobal    >>>>>>>>>>>9.99 MontoBaseDescuentoGlobal    0   TotalValorVentaNetoOpNoGravada  >>>>>>>>>>>9.99 TotalValorVentaNetoOpNoGravada  0   TotalDocumentoAnticipo  >>>>>>>>>>>9.99 TotalDocumentoAnticipo  0   MontoBaseDsctoGlobalAnticipo    >>>>>>>>>>>9.99 MontoBaseDsctoGlobalAnticipo    0   PorcentajeDsctoGlobalAnticipo   >>9.99999   PorcentajeDsctoGlobalAnticipo   0   TotalDsctoGlobalesAnticipo  >>>>>>>>>>>9.99 TotalDsctoGlobalesAnticipo  0   MontoBaseICBPER >>>>>>>>>>>9.99 MontoBaseICBPER 0   TotalMontoICBPER    >>>>>>>>>>>9.99 TotalMontoICBPER    0   TotalValorVentaNetoOpExoneradas >>>>>>>>>>>9.99 TotalValorVentaNetoOpExoneradas 0   TotalVenta  >>>>>>>>>>>9.99 TotalVenta  0   �   2 B W � � ��  ���������   �               P   P�         �   �   ��  00000  �      �    �       �   �           � �           �            �������                                    (        (        (        (        #(        +(        3(        ;(        C(        K(        S(        [(        c(        k(        s(        {(        �(                �     i  i  i  i      i  i  i      i  i  i  i  i  i      i  i  i  i      i  i  i  i  i      i  i  i  i  i  j      i  i  i  i  i      i 	 i 
 i  i  i      i  i  i  i  i      i  i  i  i  i  i      i  i  i  i  i      i  i  i  i  i  i      i  i  i      i  i  i  i  i      i  i  i  i  i  i      i  i  i  i      i  i  i  i  i     	1 	 	 	 	) 	8 	 	 	' 	( 	 	E 	 	G 	 	 	 	c 	       7   >   �  �  �  �  �  �  �          !  (  /  6  =  D  �  �  K  \  c  j  q  x    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    
    0       &  -  5  <  C  S  X  J  Q  _  f  m  t  {  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �        "  ,  6  @  J  T  [  e  o  y  �  �  �  �  �  �  �  �  �  �  �  
  (  G  a  j  y  �  �  �  �  �  �    /  M  h  x  �  �    ��                                                                              �          ����                            c    ��                   +�    #(  	 /g    (  
 �c    �'         �'         �(  $ ��    undefined   ��     0�                             ��                  �       ��  �   l   ��                        �����               ��                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       �
     B          assignFocusedWidget         �      �           LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    #      LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    5      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          K      LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    W      LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    c      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    v      LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �      LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 
      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    "      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    6      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    D      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    T      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    e      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    r      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    ~      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    d                          � ߱        �
  $  �   p
  ���                       �  /   �   �
                               3   ����p   4        $                      3   �����             T  d                  3   �����       $   �   �  ���                                                   � ߱        �  �           /    �                               3   �����   �  /    4     D  �                       3   �����   t        d                      3   �����       $                          �  �   	    �  $    �  ���                                                � ߱            u   ����  �             L  �           X  �          d  �          p  �          |                        �  �          �  �          �  �              � ߱            Z   ����   ��                         �  �    �  $      4   ����$      o   �       @                              �  L  NA  `  �  l  �  �     �     �    �    �    �    �      `     
`  4  $  H    \     p      $  �  �  ���                       �     
                    � ߱        H�    �  4  �      �      4   �����                �                      ��                  �                    䁼                       �  D  D    �  �  �      �      4   �����      $  �    ���                         @         �              � ߱                 `  p      X      4   ����X      $    �  ���                       �  @         �              � ߱        assignPageProperty                              `  H      ��                  �  �  x              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            changePage                              �  �      ��                  �  �  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �  �      ��                  �  �  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            constructObject                             �  �      ��                  �  �  �              ¼                    O   ����    e�          O   ����    R�          O   ����    ��            ��   @                            �� 
  h             4  
             ��   �             \               �� 
                 �  
         ��                            ����                            createObjects                               �  h      ��                  �  �  �              �d�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �  h      ��                  �  �  �              <e�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            destroyObject                               �  �      ��                  �  �  �              �i�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �  �      ��                  �  �  �              xp�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  �      ��                  �  �  �              �x�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               �  �      ��                  �  �                �y�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               �  �      ��                  �  �                ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            notifyPage                                �      ��                  �  �  ,              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  D           ��                            ����                            passThrough                             <  $      ��                  �  �  T              d��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             l               ��                  �           ��                            ����                            removePageNTarget                               �   |       ��                  �  �  �                3�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �              �   
             ��                  �            ��                            ����                            selectPage                              �!  �!      ��                  �  �  �!              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  "           ��                            ����                            toolbar                             #  �"      ��                  �  �   #              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8#           ��                            ����                            viewObject                              0$  $      ��                  �  �  H$              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                0%  %      ��                  �  �  H%              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `%           ��                            ����                            disablePagesInFolder    
      �%       &    �	      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �%      ,&      `&    �	      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  @&      �&      �&    �	      HANDLE, getCallerWindow �&      �&      �&    �	      HANDLE, getContainerMode    �&       '      4'    �	      CHARACTER,  getContainerTarget  '      @'      t'    �	      CHARACTER,  getContainerTargetEvents    T'      �'      �'    �	      CHARACTER,  getCurrentPage  �'      �'      �'    
      INTEGER,    getDisabledAddModeTabs  �'      (      <(     &
      CHARACTER,  getDynamicSDOProcedure  (      H(      �(  !  =
      CHARACTER,  getFilterSource `(      �(      �(  "  T
      HANDLE, getMultiInstanceActivated   �(      �(       )  #  d
      LOGICAL,    getMultiInstanceSupported   �(      )      H)  $  ~
      LOGICAL,    getNavigationSource ()      T)      �)  %  �
      CHARACTER,  getNavigationSourceEvents   h)      �)      �)  &  �
      CHARACTER,  getNavigationTarget �)      �)      *  '  �
      HANDLE, getOutMessageTarget �)      *      L*  (  �
      HANDLE, getPageNTarget  ,*      T*      �*  )  �
      CHARACTER,  getPageSource   d*      �*      �*  *  �
      HANDLE, getPrimarySdoTarget �*      �*      �*  +        HANDLE, getReEnableDataLinks    �*      +      <+  ,        CHARACTER,  getRunDOOptions +      H+      x+  -  4      CHARACTER,  getRunMultiple  X+      �+      �+  .  D      LOGICAL,    getSavedContainerMode   �+      �+      �+  /  S      CHARACTER,  getSdoForeignFields �+      ,      8,  0  i      CHARACTER,  getTopOnly  ,      D,      p,  1 
 }      LOGICAL,    getUpdateSource P,      |,      �,  2  �      CHARACTER,  getUpdateTarget �,      �,      �,  3  �      CHARACTER,  getWaitForObject    �,      �,      (-  4  �      HANDLE, getWindowTitleViewer    -      0-      h-  5  �      HANDLE, getStatusArea   H-      p-      �-  6  �      LOGICAL,    pageNTargets    �-      �-      �-  7  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �-      .      D.  8  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  $.      \.      �.  9  �      LOGICAL,INPUT h HANDLE  setCallerWindow p.      �.      �.  :        LOGICAL,INPUT h HANDLE  setContainerMode    �.      �.      $/  ;        LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  /      L/      �/  <  -      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  `/      �/      �/  =  @      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �/      �/      (0  >  O      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  0      X0      �0  ?  f      LOGICAL,INPUT pcProc CHARACTER  setFilterSource p0      �0      �0  @  }      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �0       1      41  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   1      T1      �1  B  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   p1      �1      �1  C  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �1      ,2      `2  D  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   @2      �2      �2  E  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �2      �2      3  F        LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �2      83      l3  G        LOGICAL,INPUT phObject HANDLE   setPageNTarget  L3      �3      �3  H  *      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �3      �3      4  I  9      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �3      04      d4  J  G      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    D4      �4      �4  K  [      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �4      �4       5  L  p      LOGICAL,INPUT phObject HANDLE   setRunDOOptions  5      @5      p5  M  �      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  P5      �5      �5  N  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �5      �5       6  O  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields  6      L6      �6  P  �      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  `6      �6      �6  Q 
 �      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �6      �6      (7  R  �      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget 7      L7      |7  S  �      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    \7      �7      �7  T  �      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �7      �7      ,8  U        LOGICAL,INPUT phViewer HANDLE   getObjectType   8      L8      |8  V        CHARACTER,  setStatusArea   \8      �8      �8  W  (      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             l9  T9      ��                  C  D  �9              0$�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               p:  X:      ��                  F  G  �:              �+�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                t;  \;      ��                  I  J  �;              T.�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                |<  d<      ��                  L  M  �<              �.�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �=  h=      ��                  O  Q  �=              D6�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �=           ��                            ����                            getAllFieldHandles  �8      >      L>  X  6      CHARACTER,  getAllFieldNames    ,>      X>      �>  Y  I      CHARACTER,  getCol  l>      �>      �>  Z  Z      DECIMAL,    getDefaultLayout    �>      �>       ?  [  a      CHARACTER,  getDisableOnInit    �>      ?      @?  \  r      LOGICAL,    getEnabledObjFlds    ?      L?      �?  ]  �      CHARACTER,  getEnabledObjHdls   `?      �?      �?  ^  �      CHARACTER,  getHeight   �?      �?      �?  _ 	 �      DECIMAL,    getHideOnInit   �?      @      4@  `  �      LOGICAL,    getLayoutOptions    @      @@      t@  a  �      CHARACTER,  getLayoutVariable   T@      �@      �@  b  �      CHARACTER,  getObjectEnabled    �@      �@      �@  c  �      LOGICAL,    getObjectLayout �@       A      0A  d  �      CHARACTER,  getRow  A      <A      dA  e        DECIMAL,    getWidth    DA      pA      �A  f  
      DECIMAL,    getResizeHorizontal |A      �A      �A  g        LOGICAL,    getResizeVertical   �A      �A      B  h  '      LOGICAL,    setAllFieldHandles  �A      (B      \B  i  9      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    <B      |B      �B  j  L      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �B      �B      C  k  ]      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �B      (C      \C  l  n      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   <C      |C      �C  m        LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �C      �C       D  n  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �C      $D      TD  o  �      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal 4D      xD      �D  p  �      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �D      �D      E  q  �      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �D      4E      hE  r  �      LOGICAL,    getObjectSecured    HE      tE      �E  s  �      LOGICAL,    createUiEvents  �E      �E      �E  t  �      LOGICAL,    bindServer                              �F  hF      ��                  3  4  �F              @�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �G  lG      ��                  6  7  �G              $��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �H  tH      ��                  9  :  �H               ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �I  |I      ��                  <  =  �I              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �J  �J      ��                  ?  @  �J               ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �K  �K      ��                  B  C  �K              ̃�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �L  �L      ��                  E  G  �L              ���                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �L  
         ��                            ����                            startServerObject                               �M  �M      ��                  I  J  �M              �^�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �N  �N      ��                  L  N  �N              �_�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  O           ��                            ����                            getAppService   �E      xO      �O  u        CHARACTER,  getASBound  �O      �O      �O  v 
       LOGICAL,    getAsDivision   �O      �O      P  w  !      CHARACTER,  getASHandle �O      (P      TP  x  /      HANDLE, getASHasStarted 4P      \P      �P  y  ;      LOGICAL,    getASInfo   lP      �P      �P  z 	 K      CHARACTER,  getASInitializeOnRun    �P      �P      Q  {  U      LOGICAL,    getASUsePrompt  �P      Q      DQ  |  j      LOGICAL,    getServerFileName   $Q      PQ      �Q  }  y      CHARACTER,  getServerOperatingMode  dQ      �Q      �Q  ~  �      CHARACTER,  runServerProcedure  �Q      �Q      R    �      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �Q      LR      |R  �  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   \R      �R      �R  �  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �R      �R      $S  �  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   S      DS      pS  � 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    PS      �S      �S  �  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �S      �S      T  �  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �S      <T      pT  �        LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  PT      �T      �T  �        LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �U  pU      ��                      �U              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �U             �U  
             ��   V             �U               �� 
                 V  
         ��                            ����                            addMessage                               W  �V      ��                      W              dV�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   dW             0W               ��   �W             XW               ��                  �W           ��                            ����                            adjustTabOrder                              |X  dX      ��                    !  �X              �m�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �X             �X  
             �� 
  Y             �X  
             ��                  �X           ��                            ����                            applyEntry                              �Y  �Y      ��                  #  %  Z              Lq�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  $Z           ��                            ����                            changeCursor                                 [  [      ��                  '  )  8[              �u�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  P[           ��                            ����                            createControls                              L\  4\      ��                  +  ,  d\              z�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               P]  8]      ��                  .  /  h]              �~�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                T^  <^      ��                  1  2  l^              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              `_  H_      ��                  4  5  x_              芹                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              ``  H`      ��                  7  8  x`              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              `a  Ha      ��                  :  ;  xa              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                hb  Pb      ��                  =  >  �b              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              pc  Xc      ��                  @  E  �c              <�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �c             �c  
             ��   �c             �c               ��   $d             �c               ��                  d           ��                            ����                            modifyUserLinks                             e  �d      ��                  G  K  ,e               ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   xe             De               ��   �e             le               �� 
                 �e  
         ��                            ����                            removeAllLinks                              �f  xf      ��                  M  N  �f              �{�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �g  xg      ��                  P  T  �g              �{�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �g             �g  
             ��   h             �g               �� 
                 h  
         ��                            ����                            repositionObject                                i  �h      ��                  V  Y  (i              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   ti             @i               ��                  hi           ��                            ����                            returnFocus                             `j  Hj      ��                  [  ]  xj              �0�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �j  
         ��                            ����                            showMessageProcedure                                �k  |k      ��                  _  b  �k              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �k             �k               ��                  �k           ��                            ����                            toggleData                              �l  �l      ��                  d  f  �l              8+�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  m           ��                            ����                            viewObject                              n  �m      ��                  h  i  $n              h#�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �T      |n      �n  � 
 �      LOGICAL,    assignLinkProperty  �n      �n      �n  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �n      @o      po  �  �      CHARACTER,  getChildDataKey Po      |o      �o  �  �      CHARACTER,  getContainerHandle  �o      �o      �o  �  �      HANDLE, getContainerHidden  �o      �o      (p  �  �      LOGICAL,    getContainerSource  p      4p      hp  �  �      HANDLE, getContainerSourceEvents    Hp      pp      �p  �  �      CHARACTER,  getContainerType    �p      �p      �p  �        CHARACTER,  getDataLinksEnabled �p      �p      ,q  �  !      LOGICAL,    getDataSource   q      8q      hq  �  5      HANDLE, getDataSourceEvents Hq      pq      �q  �  C      CHARACTER,  getDataSourceNames  �q      �q      �q  �  W      CHARACTER,  getDataTarget   �q      �q       r  �  j      CHARACTER,  getDataTargetEvents  r      ,r      `r  �  x      CHARACTER,  getDBAware  @r      lr      �r  � 
 �      LOGICAL,    getDesignDataObject xr      �r      �r  �  �      CHARACTER,  getDynamicObject    �r      �r      s  �  �      LOGICAL,    getInstanceProperties   �r      $s      \s  �  �      CHARACTER,  getLogicalObjectName    <s      hs      �s  �  �      CHARACTER,  getLogicalVersion   �s      �s      �s  �  �      CHARACTER,  getObjectHidden �s      �s      t  �  �      LOGICAL,    getObjectInitialized    �s      (t      `t  �  	      LOGICAL,    getObjectName   @t      lt      �t  �        CHARACTER,  getObjectPage   |t      �t      �t  �  ,      INTEGER,    getObjectParent �t      �t      u  �  :      HANDLE, getObjectVersion    �t      u      Pu  �  J      CHARACTER,  getObjectVersionNumber  0u      \u      �u  �  [      CHARACTER,  getParentDataKey    tu      �u      �u  �  r      CHARACTER,  getPassThroughLinks �u      �u      v  �  �      CHARACTER,  getPhysicalObjectName   �u       v      Xv  �  �      CHARACTER,  getPhysicalVersion  8v      dv      �v  �  �      CHARACTER,  getPropertyDialog   xv      �v      �v  �  �      CHARACTER,  getQueryObject  �v      �v      w  �  �      LOGICAL,    getRunAttribute �v       w      Pw  �  �      CHARACTER,  getSupportedLinks   0w      \w      �w  �  �      CHARACTER,  getTranslatableProperties   pw      �w      �w  �        CHARACTER,  getUIBMode  �w      �w      x  � 
       CHARACTER,  getUserProperty �w      x      Lx  �  (      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    ,x      tx      �x  �  8      CHARACTER,INPUT pcPropList CHARACTER    linkHandles �x      �x       y  �  M      CHARACTER,INPUT pcLink CHARACTER    linkProperty    �x      $y      Ty  �  Y      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry 4y      �y      �y  �  f      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �y      (z      Xz  �  r      CHARACTER,INPUT piMessage INTEGER   propertyType    8z      |z      �z  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  �z      �z      {  �  �      CHARACTER,  setChildDataKey �z      {      @{  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden   {      h{      �{  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  |{      �{      �{  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �{      |      L|  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled ,|      p|      �|  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   �|      �|      �|  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �|      }      P}  �        LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  0}      x}      �}  �  !      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   �}      �}      ~  �  4      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �}      (~      \~  �  B      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  <~      �~      �~  � 
 V      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject �~      �~         �  a      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    �~      (      \  �  u      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   <      x      �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    �      �      �  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   �      (�      \�  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   <�      ��      ��  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��      Ѐ       �  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    ��       �      T�  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    4�      |�      ��  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      ؁      �  �        LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �      ,�      d�  �        LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  D�      ��      ��  �  -      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute ��      ܂      �  �  @      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �      4�      h�  �  P      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   H�      ��      ȃ  �  b      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      �      �  � 
 |      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      8�      h�  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage H�      ��      Ԅ  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   ��      ��      $�  � 	 �      CHARACTER,INPUT pcName CHARACTER    �      d�  ��      �      4   �����                ��                      ��                  �  �                  lѹ                       �  t�        �  �  ��      �      4   �����                ��                      ��                  �  �                  ���                       �  �  ��    �  ��  0�      �      4   �����                @�                      ��                  �  �                  (�                       �  Ć         �                                  �     
                    � ߱        ć  $  �  l�  ���                           $  �  ��  ���                                                � ߱        (�    �  8�  ��      ,      4   ����,                Ĉ                      ��                  �  v	                  ��                       �  H�  ��  o   �      ,                                 P�  $   �  $�  ���                       �  @         �              � ߱        d�  �   �  �      x�  �   �  4      ��  �   �  �      ��  �   �        ��  �   �  �      ȉ  �   �  	      ܉  �   �  �	      ��  �   �  �	      �  �   �  0
      �  �   �  �
      ,�  �   �         @�  �   �  �      T�  �   �        h�  �   �  T      |�  �   �  �      ��  �   �  D      ��  �   �  �      ��  �   �  �      ̊  �   �  0      ��  �   �  �      �  �   �        �  �   �  �      �  �   �        0�  �   �  �      D�  �   �         X�  �   �  t      l�  �   �  �      ��  �   �  $      ��  �   �  �      ��  �   �  �      ��  �   �  H      Ћ  �   �  �      �  �   �  �      ��  �   �  �      �  �   �  8       �  �   �  �      4�  �   �  �      H�  �   �  ,      \�  �   �  h      p�  �   �  �      ��  �   �  �      ��  �    	        ��  �   	  X      ��  �   	  �          �   	  �                      �          X�  @�      ��                  �	  �	  p�              L��                    O   ����    e�          O   ����    R�          O   ����    ��      @     
                �                     �                         � ߱        �  $ �	  ��  ���                           O   �	  ��  ��                 ��          t�  |�    d�                                             ��                            ����                                L8      Ԍ      0�     6     ��                      V ��                       �    �	  D�  ��            4   ����                Џ                      ��                  �	  r
                  T��                       �	  T�  �  �   �	  x      ��  �   �	  �      �  �   �	  h       �  �   �	  �      4�  �   �	  `      H�  �   �	  �      \�  �   �	  P      p�  �   �	  �      ��  �   �	  H      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      Ԑ  �   �	  0          �   �	  �      ��    }
  �  ��             4   ����                 ��                      ��                  ~
                    ���                       ~
  �  ��  �   �
  |       ��  �   �
  �       ̑  �   �
  d!      ��  �   �
  �!      ��  �   �
  T"      �  �   �
  �"      �  �   �
  D#      0�  �   �
  �#      D�  �   �
  ,$      X�  �   �
  �$      l�  �   �
  %      ��  �   �
  �%      ��  �   �
  &      ��  �   �
  �&      ��  �   �
  �&      В  �   �
  x'      �  �   �
  �'      ��  �   �
  p(      �  �   �
  �(       �  �   �
  h)      4�  �   �
  �)      H�  �   �
  `*      \�  �   �
  �*      p�  �   �
  X+      ��  �   �
  �+      ��  �   �
  P,      ��  �   �
  �,          �   �
  H-      ܘ      ܓ  X�      �-      4   �����-                h�                      ��                    �                  ���                         �  |�  �     .      ��  �     �.      ��  �     /      ��  �     |/      ̔  �   !  �/      ��  �   "  d0      ��  �   $  �0      �  �   %  1      �  �   &  �1      0�  �   '  �1      D�  �   (   2      X�  �   )  t2      l�  �   *  �2      ��  �   +  d3      ��  �   -  �3      ��  �   .  L4      ��  �   /  �4      Е  �   0  <5      �  �   1  �5      ��  �   2  �5      �  �   4  h6       �  �   5  �6      4�  �   6  P7      H�  �   7  �7      \�  �   8  �7      p�  �   9  D8      ��  �   :  �8      ��  �   ;  �8      ��  �   <  �8      ��  �   =  49      Ԗ  �   >  p9      �  �   ?  �9      ��  �   @  �9      �  �   B  \:      $�  �   C  �:      8�  �   D  �:      L�  �   E  ;      `�  �   F  L;      t�  �   G  �;      ��  �   H  �;      ��  �   I   <      ��  �   J  t<      ė  �   K  �<      ؗ  �   L  \=      �  �   M  �=       �  �   N  L>      �  �   O  �>      (�  �   P  D?      <�  �   Q  �?      P�  �   R  <@      d�  �   S  �@      x�  �   T  �@      ��  �   U  pA      ��  �   V  �A      ��  �   W  �A      Ș  �   X  $B          �   Y  �B      4�  $  �  �  ���                        C     
                    � ߱        ̙      P�  `�      C      4   ����C      /     ��     ��                          3   ����C            ��                      3   ����<C   �      �  d�  P�  XC      4   ����XC  	              t�                      ��             	       �                  �}�                         ��  ��  �     �C      ��  $    ��  ���                       �C     
                    � ߱        ��  �      D      L�  $   "   �  ���                       ,D  @         D              � ߱        �  $  %  x�  ���                       �D                         � ߱        �D     
                pE                     �F  @        
 �F              � ߱        ��  V   /  ��  ���                        �F                      G                     <G                         � ߱        (�  $  K  4�  ���                       �G     
                xH                     �I  @        
 �I              � ߱        ��  V   ]  Ĝ  ���                        �I     
                PJ                     �K  @        
 `K              � ߱            V   �  T�  ���                        
              �                      ��             
     �  =                  �k�                       �  �  �K     
                (L                     xM  @        
 8M          �M  @        
 �M          <N  @        
 �M          �N  @        
 \N              � ߱            V   �  `�  ���                        adm-clone-props ̎  D�              �     7     `                          \  �#                     start-super-proc    T�  ��  �           �     8                                  $                     ��    U  <�  L�      (R      4   ����(R      /   V  x�     ��                          3   ����8R            ��                      3   ����XR  �  $  p  �  ���                       xR                         � ߱        ̢    �  ,�  ��  H�  �R      4   �����R                �                      ��                  �  �                  �Z�                       �  <�  �R                     �R                     �R                         � ߱            $  �  ��  ���                             �  d�  ��      �R      4   �����R  S                         � ߱            $  �  t�  ���                       ȣ    �  �  ��  P�  S      4   ����S      $  �  $�  ���                       <S                         � ߱            �   �  PS      �S     
                T                     \U  @        
 U              � ߱        ��  V   �  d�  ���                        �  �   �  hU      ��    u  $�  4�      �U      4   �����U      /   v  `�     p�                          3   �����U            ��                      3   �����U  \�  $  z  ̤  ���                       �U                         � ߱         V     
                �V                     �W  @        
 �W              � ߱        ��  V   �  ��  ���                        h�    �  ��   �      �W      4   �����W                0�                      ��                                       $��                          ��      g     H�         k��                           �          �  Ȧ      ��                        ��              ���                    O   ����    e�          O   ����    R�          O   ����    ��          /    <�     L�   X                      3   ����X  |�     
   l�                      3   ����,X         
   ��                      3   ����4X    ��                              ��        �                  ����                                        \�              9      ��                      g                               p�  g     ��          k�	�                           H�          �   �      ��                      0�              �x�                    O   ����    e�          O   ����    R�          O   ����    ��          /    t�     ��  XX                      3   ����<X            ��                      3   ����`X    ��                              ��        �                  ����                                        ��              :      ��                      g                               x�  g   	  ��          k�	�                           P�           �  �      ��                  	    8�              dy�                    O   ����    e�          O   ����    R�          O   ����    ��          /  
  |�     ��  �X                      3   ����|X            ��                      3   �����X    ��                              ��        �                  ����                                        ��              ;      ��                      g                               ذ    "  ��  �      �X      4   �����X                 �                      ��                  #  B                  ���                       #  ��  ��  /   $  L�     \�                          3   �����X            |�                      3   �����X  ��  /  &  ��     ȭ  (Y                      3   ����Y  ��     
   �                      3   ����0Y  (�        �                      3   ����8Y  X�        H�                      3   ����LY            x�                      3   ����pY  ��    .  ��  ��      �Y      4   �����Y      /  4  �     �  Z                      3   �����Y   �     
   �                      3   ����$Z  P�        @�                      3   ����,Z  ��        p�                      3   ����@Z            ��                      3   ����dZ        :  ̯  ܯ      �Z      4   �����Z      /  =  �     �  �Z                      3   �����Z  H�     
   8�                      3   �����Z  x�        h�                      3   �����Z  ��        ��                      3   �����Z            Ȱ                      3   ����[  ��    F  ��  p�      <[      4   ����<[                ��                      ��                  G  J                  ���                       G  �      g   H  ��         k�<�        L[                  `�          0�  �      ��                  I      H�              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  I  ��     ��  p[                      3   ����X[  ̲     
   ��                      3   ����|[         
   �                      3   �����[    ��                            ����                                        ��              <      ��                      g                               0�     N  �[                                     �[     
                \                     l]  @        
 ,]              � ߱        ��  V   �  ̳  ���                        �]     
                �]                     L_  @        
 _              � ߱        �  V   �  \�  ���                        p�      �  �      `_      4   ����`_      $     D�  ���                       �_  @         �_              � ߱        D�  g   C  ��         k��        �_  k��        �_                  d�          4�  �      ��                  D  I  L�              XL�                    O   ����    e�          O   ����    R�          O   ����    ��            H  ��  ��      �_      4   �����_      O  H  ������   `    ��                            ����                                        ��              =      ��                      g                               �  g   P  \�         k6��         `                  $�          ��  ܷ      ��                  Q  V  �              �L�                    O   ����    e�          O   ����    R�          O   ����    ��      <�    T   `  }          O  U  ������  4`    ��                            ����                                        p�              >      T�                      g                                �  g   ^  �         k"��                           й          ��  ��      ��                  _  i  ��              �M�                    O   ����    e�          O   ����    R�          O   ����    ��      �  	  `  �                         T`            3   ����H`  x�  V   `  @�  ���                                                    ߱                    ��    b  ��  ��      ``      4   ����``      O  b  ������  �`  ��  /   d  �                                 3   �����`  <�    e  �  $�      �`      4   �����`      O  e    ����  �`      s   g  h�       ؿ                  �  ��  �       ��                            7   ����           ��                `a   �            4�                  6   g         X�   ��               `a   �            4�                                                                ��  ��            a  0a  @a  Pa           (a  8a  Ha  Xa                      t�   ��          b  b  $b  0b  <b          ��  �  l�       ��                            7   ����          ��               �b   �            ��                  6   g        ��   ��         �  �b   �            ��                                                       	 \b  
 hb   tb                 P�  D�           �b  �b  �b           �b  �b  �b         �            �   ,�          $c  0c  <c              ��  ��       ��`                           7   ����         ����               Tc   �            L�                  6   g        |�  ����         p�  Tc   �            L�                                                        Hc                 Ŀ  ��                                   @            ��   ��           �`   �`   �`   a   a   Hb                  8�                                           ��                              ��        �                  ����                            c                         +�    ğ          �  �         ?     @�             ��      g   <�                          <�  g   q  8�         k"��                            �          ��  ��      ��                 r  ~  ��              t��                    O   ����    e�          O   ����    R�          O   ����    ��      D�  	  s  4�                         |c            3   ����pc  ��  V   s  p�  ���                                                     ߱                    ��    u  ��  ��      �c      4   �����c      O  u  ������  �c  (�  /   w  �                                 3   �����c  X�    x  D�  ��      �c      4   �����c                ��                      ��                  x  {                  ��                       x  T�  @�    y  ��  ��      �c      4   �����c      	  y  0�                                        3   ����d      O  z  ������  $d      s   }  ��       ��                  �  ��   �       ��                            7   ����           ��                �d   �            P�                  6   }         t�   ��               �d   �            P�                                                                ��  ��           td  �d  �d  �d           |d  �d  �d  �d                      ��   ��          `e  le  xe  �e  �e          ��  8�  ��       ��                            7   ����          ��               f   �            ��                  6   }        �   ��         ��  f   �            ��                                                       	 �e  
 �e   �e                 l�  `�           �e  �e  �e           �e  �e  �e         �            0�   H�          xf  �f  �f              ��  �       ��`                           7   ����         ����               �f   �            h�                  6   }        ��  ����         ��  �f   �            h�                                                        �f                 ��  ��                                   @            ��   ��           8d   Dd   Pd   \d   hd   �e                   T�                                            ��                              ��        �                  ����                            c                         +�    ��          L�   �          @     \�             ��      g   X�                          �  g   �  T�         k4��                            �          ��  ��      ��                  �  �  �              8��                    O   ����    e�          O   ����    R�          O   ����    ��      t�  $  �  H�  ���                       �f                         � ߱            s   �  ��       �                  (�  ��  �       ��                            7   ����           ��                Tg   �            l�                  6   �         ��   ��               Tg   �            l�                                                                ��  ��           g  $g  4g  Dg           g  ,g  <g  Lg                      ��   ��           h  h  h  $h  0h          ��  T�  ��       ��                            7   ����          ��               �h   �            ��                  6   �        0�   ��         �  �h   �            ��                                                       	 Ph  
 \h   hh                 ��  |�           th  �h  �h           |h  �h  �h         �            L�   d�          i  $i  0i              ��  4�       ��`                           7   ����         ����               Hi   �            ��                  6   �        ��  ����         ��  Hi   �            ��                                                        <i                 ��  ��                                   @            ��   ��           �f   �f   �f   �f   g   <h      ��                              ��        �                  ����                            c                         +�                h�              A      <�             ��      g                               ��  g   �  4�         k4��                           ��          ��  ��      ��                  �  �  ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��      T�  $  �  (�  ���                       di                         � ߱            s   �  ��       ��                  �  ��  ��       ��                            7   ����           ��                �i   �            L�                  6   �         p�   ��               �i   �            L�                                                                ��  ��           �i  �i  �i  �i           �i  �i  �i  �i                      ��   ��          �j  �j  �j  �j  �j          ��  4�  ��       ��                            7   ����          ��               Dk   �            ��                  6   �        �   ��         ��  Dk   �            ��                                                       	 �j  
 �j   k                 h�  \�           k  $k  4k           k  ,k  <k         �            ,�   D�          �k  �k  �k              ��  �       ��`                           7   ����         ����               �k   �            d�                  6   �        ��  ����         ��  �k   �            d�                                                        �k                 ��  ��                                   @            ��   ��           xi   �i   �i   �i   �i   �j      ��                              ��        �                  ����                            c                         +�                H�              B      �             |�      g                                     �  �  ��      l      4   ����l                �                      ��                  �  �                  �                       �  (�  l  @                     @l  @         ,l          hl  @         Tl              � ߱        4�  $   �  ��  ���                       0�  g   �  L�         kn��      }                      �          ��  ��      ��                  �  �  ��              p�                    O   ����    e�          O   ����    R�          O   ����    ��      P�  /  �  @�                                 3   ����tl        �  l�  |�      �l      4   �����l      O  �  ������  �l    ��                            ����                                        `�              C      ��                      g                               �  g   �  H�         k!��         �l                  <�          ��  ��      ��                  �  �  ��              �                    O   ����    e�          O   ����    R�          O   ����    ��      �l  @                         � ߱            $  �  �  ���                         ��                            ����                                        \�              D      h�                      g                               @�  /   �  0�                                 3   �����l        �  \�  ��      m      4   ����m                T�                      ��                  �  �                  H�                       �  l�                ��          |�  d�      ��                 �  �                  ��                       �  ��      O   �    ��          O   �    ��      ��  /   �  ��                                 3   ���� m        �  ��  ��      @m      4   ����@m      k   �  �              }       n        �   adm-create-objects  ��  0�                      E      �                               &                     Aprobar-Rebate  D�  ��          �$  �$  " ! F     �%             X&          �%  '                     disable_UI  ��  �                      G      <                              "'  
                   enable_UI   �  t�                      H      �             �              -'  	                   exitObject  ��  ��                      I      �                               7'  
                   Extorna-AC  ��  D�                      J      �                               B'  
                   Graba-Temp-FeLogErrores P�  ��                      K      �                              Z'                     initializeObject    ��   �                      L      4                              �'                     Rechazar-Rebate 4�  ��              �    % M     �                          �  �'                     validar ��  ��  �       �  �  & ' N     0                          ,  �'                      � ���  �      
999-999999 ��OTROS ���  �        	 
    ��  8   ����$   ��  8   ����$   ��  8   ����#   ��  8   ����#   ��  8   ����   �  8   ����   �  8   ����   ,�  8   ����   <�  8   ����
   L�  8   ����
   \�  8   ����	   l�  8   ����	       8   ����       8   ����       ��  ��      toggleData  ,INPUT plEnabled LOGICAL    |�  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��   �  ,�      returnFocus ,INPUT hTarget HANDLE   �  T�  h�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    D�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �  �      removeAllLinks  ,   ��  (�  8�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE �  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  �  (�      hideObject  ,   �  <�  T�      editInstanceProperties  ,   ,�  h�  x�      displayLinks    ,   X�  ��  ��      createControls  ,   |�  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  $�  4�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER �  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER |�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  L�  \�      unbindServer    ,INPUT pcMode CHARACTER <�  ��  ��      startServerObject   ,   t�  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  ��  ��      restartServerObject ,   ��  �  (�      initializeServerObject  ,    �  <�  P�      disconnectObject    ,   ,�  d�  x�      destroyServerObject ,   T�  ��  ��      bindServer  ,   |�  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  ��  ��      enableObject    ,   ��  �  �      disableObject   ,   ��  0�  <�      applyLayout ,    �  P�  \�      viewPage    ,INPUT piPageNum INTEGER    @�  ��  ��      viewObject  ,   x�  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  �  (�      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER �  d�  p�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  T�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  $�  @�      initializeVisualContainer   ,   �  T�  `�      hidePage    ,INPUT piPageNum INTEGER    D�  ��  ��      destroyObject   ,   |�  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  ��  ��      createObjects   ,   ��  �  �      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  ��  ��      changePage  ,   ��  ��   �      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER (P˃��6{-�]Y�qh(P˃��6{-�]Y�qh))     ��M�┞I�T����M�┞I�T��)      {���v�`*Ļ*�k�ʣ{���v�`*Ļ*�k�ʣ)     �w3�&#t�`�����w3�&#t�`�����(      
 �&  0    �� ����  lc4���[/[�������D�ne6 G     � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
   � �   �%     sunat\p-formato-doc � �     "      %     ccb\libreria-ccb k�%$     usuario-concepto-permitido 
"   
   "       
"   
   %              � �   RO� #   RO�    RO" 	     " 	     " 	     " 	     ((       " 	     %              � �     � �     " 	     " 	     " 
     %              %              %                  �     }        �G� Z	   �G%              � ^	  /   %          %       %       	 %       	%       	 %       	%               %               %               %              %              %              %               %              
�        
"   
 �
�    
"   
 �
"   
 �    �        �     �        �    
"   
   �        �         �     }        �%              
"   
 �
"   
 �    �        @     �        L    
"   
   �        �         �     }        �%              � 
"    
 �%              � �  �         �      T     @     $              
�    � �   �     
"   
 �� �   �     
�             �G                      
�            � �   �
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 ��           �    1� �  
 �� �   �%               o%   o           � �    �
"   
 ��           (    1� �   �� �   �%               o%   o           � �   �
"   
 ��           �    1� �  
 �� �   �%               o%   o           � �   �
"   
 ��               1� �   �� �   �%               o%   o           � 
   �
"   
 ��           �    1�    �� �   �%               o%   o           �     �
"   
 ��           �    1� 7   �� C   �%               o%   o           %               
"   
 ��          t	    1� K   �� [     
"   
 ��           �	    1� b   �� �   �%               o%   o           � u  e �
"   
 ��           $
    1� �   �� �   �%               o%   o           � �  [ �
"   
 ��           �
    1� F   �� C   �%               o%   o           %               
"   
 ��               1� V   �� C   �%               o%   o           %               
"   
 ��           �    1� h   �� C   �%               o%   o           %              
"   
 ��              1� u   �� C     
"   
 ��           H    1� �  
 �� C   �%               o%   o           %               
"   
 ��           �    1� �   �� �   �%               o%   o           � �    �
"   
 ��          8    1� �   �� [     
"   
 ��           t    1� �   �� �   �%               o%   o           � �  t �
"   
 ��          �    1� 2  
 �� [     
"   
 ��           $    1� =   �� �   �%               o%   o           � N  � �
"   
 ��           �    1� �   �� �   �%               o%   o           � �    �
"   
 ��               1� �  
 �� �   �%               o%   o           %               
"   
 ��           �    1�    �� C   �%               o%   o           %               
"   
 ��               1� 	   �� �   �%               o%   o           � �    �
"   
 ��           x    1�    �� �   �%               o%   o           o%   o           
"   
 ��           �    1� *  
 �� �   �%               o%   o           � �    �
"   
 ��           h    1� 5   �� F  	 �%               o%   o           � P  / �
"   
 ��          �    1� �   �� F  	   
"   
 ��               1� �   �� F  	 �o%   o           o%   o           � �    �
"   
 ��          �    1� �   �� F  	   
"   
 ��           �    1� �   �� F  	 �o%   o           o%   o           � �    �
"   
 ��          <    1� �   �� C     
"   
 ��          x    1� �   �� F  	   
"   
 ��          �    1� �   �� F  	   
"   
 ��          �    1� �   �� F  	   
"   
 ��           ,    1� �   �� C   �o%   o           o%   o           %              
"   
 ��          �    1�    �� F  	   
"   
 ��          �    1�   
 �� $     
"   
 ��               1� ,   �� F  	   
"   
 ��          \    1� ;   �� F  	   
"   
 ��          �    1� N   �� F  	   
"   
 ��          �    1� c   �� F  	   
"   
 ��              1� r  	 �� F  	   
"   
 ��          L    1� |   �� F  	   
"   
 ��          �    1� �   �� F  	   
"   
 ��           �    1� �   �� �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
 �(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�       �    �� �     p�               �L
�    %              � 8      �    � $         � �          
�    � �     
"   
 �� @  , 
�       �    �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ��           l    1� �  
 �� �   �%               o%   o           � �    �
"   
 ��           �    1� �  
 �� �   �%               o%   o           o%   o           
"   
 ��           \    1� �   �� [   �%               o%   o           o%   o           
"   
 ��           �    1� �   �� C   �%               o%   o           %               
"   
 ��           T    1�    �� C   �%               o%   o           %               
"   
 ��           �    1�    �� �   �%               o%   o           � �    �
"   
 ��           D    1� !   �� C   �%               o%   o           %              
"   
 ��           �    1� 3   �� C   �%               o%   o           o%   o           
"   
 ��           <    1� ?   �� �   �%               o%   o           o%   o           
"   
 ��           �    1� M  	 �� �   �%               o%   o           � �    �
"   
 ��           ,    1� W   �� �   �%               o%   o           o%   o           
"   
 ��           �    1� k   �� �   �%               o%   o           o%   o           
"   
 ��           $    1� z   �� C   �%               o%   o           %               
"   
 ��           �    1� �   �� C   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 ��           p     1� �   �� F  	 �%               o%   o           � �    �
"   
 ��           �     1� �   �� F  	 �%               o%   o           � �    �
"   
 ��           X!    1� �   �� C   �%               o%   o           %               
"   
 ��           �!    1� �   �� F  	 �%               o%   o           � �    �
"   
 ��           H"    1� �   �� F  	 �%               o%   o           � �    �
"   
 ��           �"    1� �   �� C   �%               o%   o           %               
"   
 ��           8#    1� �   �� F  	 �%               o%   o           � �    �
"   
 ��           �#    1� �   �� F  	 �%               o%   o           � �    �
"   
 ��            $    1�    �� F  	 �%               o%   o           � �    �
"   
 ��           �$    1�    �� F  	 �%               o%   o           o%   o           
"   
 ��           %    1� $   �� F  	 �%               o%   o           � �    �
"   
 ��           �%    1� 4   �� F  	 �%               o%   o           � �    �
"   
 ��           �%    1� B  	 �� $   �%               o%   o           %               
"   
 ��           t&    1� L   �� $   �%               o%   o           %               
"   
 ��           �&    1� U   �� C   �%               o%   o           o%   o           
"   
 ��           l'    1� f   �� C   �%               o%   o           o%   o           
"   
 ��           �'    1� u   �� C   �%               o%   o           %               
"   
 ��           d(    1� �   �� C   �%               o%   o           %               
"   
 ��           �(    1� �   �� C   �%               o%   o           %               
"   
 ��           \)    1� �   �� �   �%               o%   o           %       
       
"   
 ��           �)    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           T*    1� �   �� �   �%               o%   o           %              
"   
 ��           �*    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           L+    1� �   �� �   �%               o%   o           %              
"   
 ��           �+    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           D,    1� �   �� �   �%               o%   o           %              
"   
 ��           �,    1�    �� �   �%               o%   o           o%   o           
"   
 ��           <-    1�    �� F  	 �%               o%   o           � �    �P �L 
�H T   %              �     }        �GG %              
"   
 ��           .    1�    �� �   �%               o%   o           %               
"   
 ��           �.    1� )   �� �   �%               o%   o           o%   o           
"   
 ��           �.    1� 5   �� �   �%               o%   o           � �    �
"   
 ��           p/    1� E   �� �   �%               o%   o           � [  - �
"   
 ��           �/    1� �   �� �   �%               o%   o           � �    �
"   
 ��           X0    1� �   �� �   �%               o%   o           � �   �
"   
 ��          �0    1� �   �� [     
"   
 ��           1    1� �   �� �   �%               o%   o           � �    �
"   
 ��          |1    1� �  
 �� [     
"   
 ��          �1    1�     �� [     
"   
 ��           �1    1�     �� F  	 �%               o%   o           � �    �
"   
 ��           h2    1�     �� �   �%               o%   o           � �    �
"   
 ��           �2    1� *    �� [   �%               o%   o           o%   o           
"   
 ��           X3    1� 7    �� �   �%               o%   o           � J   ! �
"   
 ��           �3    1� l    �� �   �%               o%   o           � �    �
"   
 ��           @4    1� y    �� �   �%               o%   o           � �    �
"   
 ��           �4    1� �   	 �� �   �%               o%   o           o%   o           
"   
 ��           05    1� �    �� C   �%               o%   o           %               
"   
 ��          �5    1� �    �� [     
"   
 ��           �5    1� �    �� �   �%               o%   o           � �    �
"   
 ��           \6    1� �    �� F  	 �%               o%   o           � �    �
"   
 ��           �6    1� �    �� F  	 �%               o%   o           � �    �
"   
 ��          D7    1� �    �� [     
"   
 ��          �7    1� !   �� F  	   
"   
 ��           �7    1� $!   �� C   �o%   o           o%   o           %               
"   
 ��          88    1� ;!   �� C     
"   
 ��          t8    1� R!   �� F  	   
"   
 ��          �8    1� `!   �� F  	   
"   
 ��          �8    1� s!   �� F  	   
"   
 ��          (9    1� �!   �� F  	   
"   
 ��          d9    1� �!   �� F  	   
"   
 ��          �9    1� �!   �� [     
"   
 ��           �9    1� �!   �� �   �%               o%   o           � �!  4 �
"   
 ��          P:    1� "   �� [     
"   
 ��          �:    1� "   �� [     
"   
 ��          �:    1�  "   �� [     
"   
 ��          ;    1� -"   �� F  	   
"   
 ��          @;    1� A"   �� F  	   
"   
 ��          |;    1� S"   �� F  	   
"   
 ��          �;    1� e"   �� C     
"   
 ��           �;    1� r"   �� F  	 �%               o%   o           � �    �
"   
 ��           h<    1� �"   �� F  	 �%               o%   o           � �    �
"   
 ��           �<    1� �"   �� F  	 �%               o%   o           � �    �
"   
 ��           P=    1� �"   �� F  	 �%               o%   o           � �    �
"   
 ��           �=    1� �"   �� C   �%               o%   o           %               
"   
 ��           @>    1� �"   �� C   �%               o%   o           o%   o           
"   
 ��           �>    1� �"   �� C   �%               o%   o           %               
"   
 ��           8?    1� �"   �� C   �%               o%   o           %               
"   
 ��           �?    1� �"   �� C   �%               o%   o           o%   o           
"   
 ��           0@    1� #   �� C   �%               o%   o           %               
"   
 ��          �@    1� #   �� F  	   
"   
 ��           �@    1� )#   �� C   �%               o%   o           %              
"   
 ��          dA    1� :#   �� F  	   
"   
 ��          �A    1� F#   �� F  	   
"   
 ��          �A    1� U#  
 �� F  	   
"   
 ��           B    1� `#   �� F  	 �%               o%   o           � �"   �
"   
 ��           �B    1� r#   �� F  	 �%               o%   o           � �    �
"   
    "    �%     start-super-proc n�%     adm2/smart.p k�P �L 
�H T   %              �     }        �GG %              
"   
   �       �C    6� �     
"   
   
�        �C    8
"   
   �        �C    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        @E    �� �   � P   �        LE    �@    
� @  , 
�       XE    �� �   �p�               �L
�    %              � 8      dE    � $         � �          
�    � �   �
"   
 �p� @  , 
�       tF    �� b   �p�               �L"    , �   � �#   �� �#   ��     }        �A      |    "      � �#   �%              (<   \ (    |    �     }        �A� �#   �A"    �    "    �"    �  < "    �"    �(    |    �     }        �A� �#   �A"    �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        HH    �� �   � P   �        TH    �@    
� @  , 
�       `H    �� �   �p�               �L
�    %              � 8      lH    � $         � �          
�    � �   �
"   
 �p� @  , 
�       |I    �� �  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �         J    �� �   � P   �        ,J    �@    
� @  , 
�       8J    �� �   �p�               �L
�    %              � 8      DJ    � $         � �          
�    � �   �
"   
 �p� @  , 
�       TK    �� K   �p�               �L
"   
 , 
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        �K    �� �   � P   �        L    �@    
� @  , 
�       L    �� �     p�               �L
�    %              � 8      L    � $         � �          
�    � �     
"   
 �p� @  , 
�       ,M    �� �  
 �p�               �L%     SmartWindow 
"   
   p� @  , 
�       �M    �� �     p�               �L%      WINDOW  
"   
  p� @  , 
�       �M    �� �    p�               �L%               
"   
  p� @  , 
�       PN    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 �    �        0O    �� �   �
"   
   � 8      |O    � $         � �          
�    � �   �
"   
   �        �O    �
"   
   �       �O    /
"   
   
"   
   �        P    6� �     
"   
   
�        LP    8
"   
   �        lP    �
"   
   �       �P    �
"   
   p�    � �#   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        PQ    �A"    �A
"   
   
�        �Q    �@ � 
"   
 �"      �       }        �
"   
 �%              %                "    �%     start-super-proc n�%     adm2/appserver.p ¹�    � W$     
�    �     }        �%               %      Server  - �     }        �    "    �� �    �%                   "    �� �    �%      NONE    p�,  8         $     "    �        � q$   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �S    �� �   � P   �        �S    �@    
� @  , 
�       �S    �� �   �p�               �L
�    %              � 8       T    � $         � �          
�    � �   �
"   
 �p� @  , 
�       U    �� W   �p�               �L"    , p�,  8         $     "    �        � $   �
�     "    �%     start-super-proc m�%     adm2/visual.p ��   � �     � �$     � �$  6   
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        lV    �� �   � P   �        xV    �@    
� @  , 
�       �V    �� �   �p�               �L
�    %              � 8      �V    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �W    �� �   �p�               �L"    , � 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP k�%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc l�%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    � "%   �
�    � 4%   �A    �    � "%     
�    � @%   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    � "%   �
�    � ]%   �%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
"    
 �
"   
 �%     contextHelp 
"    
   
�    
�    %               
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        �[    �� �   � P   �        �[    �@    
� @  , 
�       \    �� �   �p�               �L
�    %              � 8      \    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�        ]    �� �    �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        �]    �� �   � P   �        �]    �@    
� @  , 
�       �]    �� �   �p�               �L
�    %              � 8      �]    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�        _    �� �"   �p�               �L%              (        �     }        �G� Z	   �G� 
"   
 �
"   
   �        �_    �%              
"   
 �
"   
 ��     }        �%               
"   
 �%      CLOSE   %               � �%  *   "          "    �%               %               %     Rechazar-Rebate     �  � �%  	 �%               � �   �� �%   �"     �"    �"    �&    &    &    &    &    &    &    &    �    d    @            "       &        "       &        "   1    &        "   8    &        "       &    "       "   1    "   8    "       "       %              "       "       "       & 	   & 	   & 
   & 
   &    &    P    ,        &        "      & 	       "      & 
       "      &    "      "      "      "     �    "      &    � �%     "           "     �%               %               %     Aprobar-Rebate      �  � �%  	 �    "    �� �%    �"      %               � �   �� �%   �"     �"    �"    �&    &    &    &    &    &    &    &    �    d    @            "       &        "       &        "   1    &        "   8    &        "       &    "       "   1    "   8    "       "       %              "       "       "       & 	   & 	   & 
   & 
   &    &    P    ,        &        "      & 	       "      & 
       "      &    "      "      "      "     �    "      &    �     }        B� �   �� �%   �"     �"    �"    �&    &    &    &    &    &    &    &    �    d    @            "       &        "       &        "   1    &        "   8    &        "       &    "       "   1    "   8    "       "       %              "       "       "       & 	   & 	   & 
   & 
   &    &    P    ,        &        "      & 	       "      & 
       "      &    "      "      "      "     �    "      &    �     }        B� �   �� �%   �"     �"    �"    �&    &    &    &    &    &    &    &    �    d    @            "       &        "       &        "   1    &        "   8    &        "       &    "       "   1    "   8    "       "       %              "       "       "       & 	   & 	   & 
   & 
   &    &    P    ,        &        "      & 	       "      & 
       "      &    "      "      "      "     �    "      &    � 
"   
 �
"   
 �
"   
 ��         l    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � �%  	   %               
"   
 �
�    %     createObjects    �     }        �%     initializeObject l� �     }        ��            �'    " "   �%              � D&  A   � �%  	   �    }        �� �&         �            �'%       ��������     " !     %       ��������    " !     %              %              � �%  	   � �%  	   �           �" !     %      validar " 	 '  �" 	 (  �" "   �    " "   �� �&   �� 	   %       ��������� �%           " !     %                  " !   �%              " !     &        � 	   &    * 	            * 	   2 	   ? 	       �     }        �%               �    }        �%                  %              %                   " !     %                  " !     �     }        ��     }        � ,          " !     G %       
       �    }        �" !   �%              " !     %              *" !  *   � 4      <q    �             ,     %                      %              " !     *�'   *" !  * � <    * 	        %                  " !     � �%    �     � �&     � �&     (        " !   �%               * 	   � �&         "    �%              %(     sunat/sunat-calculo-importes.r %     tabla-ccbcdocu  
" !  
   " 	 1  �" 	   �" 	   �"    �
" !  
       �  � �%  	 �� �%  	   %$     sunat\progress-to-ppll-v3.r " 	 1  �" 	   �" 	   �"    �%                  �  � �%  	 �� �%         �  � �&  
 �� �%          " "     %              "       +  %      Graba-Temp-FeLogErrores �    }        �� �%      � �   �� �%   �"     �"    �"    �&    &    &    &    &    &    &    &    �    d    @            "       &        "       &        "   1    &        "   8    &        "       &    "       "   1    "   8    "       "       %              "       "       "       & 	   & 	   & 
   & 
   &    &    P    ,        &        "      & 	       "      & 
       "      &    "      "      "      "     �    "      &     (         � �&          " "   �� '     (        �     }        �G� Z	   �G� 
"   
 �
"   
   �     }        �
�    
"   
 �"  	  �"  
  �
"   
 �� �   �� �%   �"     �"    �"    �&    &    &    &    &    &    &    &    �    d    @            "       &        "       &        "   1    &        "   8    &        "       &    "       "   1    "   8    "       "       %              "       "       "       & 	   & 	   & 
   & 
   &    &    P    ,        &        "      & 	       "      & 
       "      &    "      "      "      "     �    "      &    
"   
   %      CLOSE   %               * #   �           %              "     �&    &    � 4                        " $     � �'     " $     " $     %      SUPER       �            �'%       ��������     " %     %       ��������    " %     %              %              � �%  	   � �%  	   �           �" %     � 	   :       " %     %                  " %   �%              " %     &        � 	   &    * 	       2 	   �     }        �    �     }        �%               �    }        �%                  %              %                   " %     %                  " %     �     }        ��     }        � ,          " %     G %       
       �    }        �" %   �" %     (        " %   �%               * 	   � �%  	   � �'     "       +      "    �� #   �%     vta2/extorna-ac � 	       �  � �%  	 �� �%  	   %     ccb\libreria-ccb.r %, !    notas-creditos-supera-comprobante �
" '  
   " &     " &     " &     
" '  
       " &   �� �'   �� �&                     �           �   l       ��                 �  �  �               �m�                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       �N     
                    � ߱              �  (  �      <O      4   ����<O                �                      ��                  �  �                  ���                       �  8  �  �  �  �O            �  �  `      �O      4   �����O                p                      ��                  �  �                  `��                       �  �  �  o   �      ,                                 �  �   �   P      �  �   �  ,P      $  $  �  �  ���                       XP     
                    � ߱        8  �   �  xP      L  �   �  �P      `  �   �  �P          $   �  �  ���                       �P  @         �P              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  6  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      �#                      �          �  $      ���                       <Q     
                    � ߱                  �  �                      ��                     
                   ��                       4      4   ����\Q      $  	  �  ���                       �Q     
                    � ߱        �      4  D      �Q      4   �����Q      /    p                               3   �����Q  �  �   '  �Q          O   4  ��  ��  R                               , �                          
                               �      ��                            ����                                                        �   l       ��                  �  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                 �  7  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��        $    �   ���                       Xm      "                   � ߱              (  �      lm      4   ����lm                �                      ��                                      �i                         8  �  	   	  �                                          3   �����m      O   
  ��  ��  �m  $  �     �m      �  �          �        �              p      ��      0           -  �              |k    !  Ln  ����p       4      $    �  ���                       �m      !                   � ߱        `  $    4  ���                       �m      !                   � ߱            4   ����$n      O        ��  `n      O        ��  ln          �  8      xn      4   ����xn                H                      ��                    ,                  �                         �  l  /     t     �                          3   �����n  �        �                      3   �����n  �        �                      3   �����n                                3   �����n      $     @  ���                                "                   � ߱                �        �n      4   �����n                                      ��                    +                  ��                         �  l  $    @  ���                       �n      !                   � ߱        0  $  v  �  ���                       �n      !                   � ߱                      �          p  @      ��      �          x  �  X      @&  �  �q                     x  �      O   x    e�          O   x    e�          O   ����  R�      �  $  y  �  ���                       o      !                   � ߱        8  $  ~    ���                       o      !                   � ߱        |      T  d      @o      4   ����@o      O     �� ��      �  <  �      	     ����   to     �  |o                                        ho  	    �  �  	      �o      4   �����o      O   �  �� ��      \    �  8	  �	      �o      4   �����o                �	                      ��       @     	     �  �                  �x                       �  H	  X    �  �	  \
      �o      4   �����o                l
                      ��       @          �  �                  `y                       �  �	  0  $  �  �
  ���                       �o      !                   � ߱          @      �                         ��        p         �  �                  �y    !  �p            �  �
      $  �  l  ���                        p      !                   � ߱        �  $  �  �  ���                       Pp      !                   � ߱            4   ����xp      $  �  ,  ���                       �p      !                   � ߱        D    �  t  �      q      4   ����q  	                                     ��       @     	     �  �                  hz                       �  �      	  �  4                                        3   ����q      O   �  �� ��             �  (q                                     
                            �      ��      @          �  �                  �|                    O   ����
  ��      l  $  �  @  ���                       Hq      !                   � ߱        X    �  �        �q      4   �����q                                      ��       @          �  �                  �}                       �  �      	  �  H                                        3   �����q  h  �   �  �q      O  �  �� e�                                  �      ��      H          �  �                  ~                    O   ����  ��            �     �      �q      4   �����q                �                      ��       @          �  �                  �~                       �  0      $  �  �  ���                       r      !                   � ߱        t    �     0      8r      4   ����8r      O   �    ��      tr      	                   � ߱        �  V   �  H  ���                        �  �   �     <    �  �  H      �r      4   �����r                X                      ��                  �                    ܈                       �  �  �  /  �  �         !                      3   �����r  �  /  �  �     �  �r                      3   �����r           �                      3   �����r  0                               3   ����s  `        P                      3   ����s            �  �                  3   ���� s      $   �  �  ���                                                   � ߱        �  �     ,s            $      8s      4   ����8s      O        ��  Ps  �  /   
  h     x                          3   ����\s  �        �                      3   �����s  �        �                      3   �����s          �                      3   �����s  (  $                                    H  X                  3   �����s      $   
  �  ���                                                   � ߱        �  p     �s  �     &  �  H     �s                �                      ��                                      ̊                         �  �s      	                   � ߱            V     X  ���                            ,     �s                h                      ��                                      H�                         �  t      	                   � ߱            V     <  ���                                                            ��                     %                  ċ                          �  �  $  !  <  ���                       t      "                   � ߱        8t      	 T       T       Dt      	 [       [           � ߱            V   "  h  ���                          /   '                                   3   ����Ht  h  F   )       	       ��                                                        �   *  	      �  ~       
  L L L      O O O      0 0 0      * * *              
 
 
      g g g                                      D D D      1 1 1              B B B              ! ! !      = = =              C C C      ' ' '      " " "      i i i      h h h      k k k      j j j      t t t              c c c      4 4 4      @ @ @      H H H      % % %      ; ; ;              a a a      ] ] ]              - - -      . . .      ) ) )      G G G      : : :              E E E      P P P              / / /      5 5 5      + + +      , , ,      3 3 3      & & &      f f f                      K K K              e e e              I I I              2 2 2      # # #      d d d              J J J      $ $ $      T T T      U U U      V V V      W W W      X X X      Y Y Y      Z Z Z      [ [ [      \ \ \      ^ ^ ^      _ _ _      ` ` `      l l l              F F F      v v v      y y y      | | |      R R R              ? ? ?      M M M              > > >                      ( ( (      < < <      u u u      z z z      7 7 7              Q Q Q      	 	 	              S S S   
  N N N                 6 6 6      x x x      { { {      p p p      q q q      } } }      s s s      o o o      r r r      ~ ~ ~      n n n      m m m      w w w                      8 8 8      b b b      9 9 9      A A A           �  �   /  lt      L$  s   3  �        $                  8!  �  ,        ��                            7   ����           ��                u   �            |                   6   3         �    ��               u   �            |                                                                  !  �            �t  �t  �t  �t           �t  �t  �t   u                      �    �           �u  �u  �u  �u  �u          �"  d!  �!       ��                            7   ����          ��               Xv   �            "                  6   3        @"   ��         ("  Xv   �            "                                                       	 v  
 v   v                 �"  �"           (v  8v  Hv           0v  @v  Pv         �            \"   t"          �v  �v  �v              �"  D#       ��`                           7   ����         ����               �v   �            �#                  6   3        �#  ����         �#  �v   �            �#                                                        �v                 $   $                                   @            �#   �#           �t   �t   �t   �t   �t   �u        	   5  �$                                          3   ����w              "  �$                                  �%         !  �%          p%  �%    �  %                                                                                         *             
             	     0   @   P   `   p   �      	     0   @   P   `   p   �    �   ��   ! "   ��                              ��        �                   ��                            ����                                �    T&  !      c                         +�                    �           �   l       ��                  =  J  �               $�                    O   ����    e�          O   ����    R�          O   ����    ��           G  �   �       Tw      4   ����Tw      n   H     �          �w        I    ,      �w      4   �����w      �   I  �w    ��                            ����                                            �           �   l       ��                  P  `  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      �w  �           �w  �              � ߱        p  Z   Z  �    �        �w                  �               �              �              �              �              � ߱        �  h   \     �        �w              d  s   ^  �       8                  P  �  D       ��                            7   ����           ��                hx   �            �                  6   ^         �   ��               hx   �            �                                                                             (x  8x  Hx  Xx           0x  @x  Px  `x                      �   �          y   y  ,y  8y  Dy          �  |  �       ��                            7   ����          ��               �y   �                              6   ^        X   ��         @  �y   �                                                                   	 dy  
 py   |y                 �  �           �y  �y  �y           �y  �y  �y         �            t   �          ,z  8z  Dz                \       ��`                           7   ����         ����               \z   �            �                  6   ^        �  ����         �  \z   �            �                                                        Pz                 $                                     @            �              �w   �w   x   x   x   Py        
   _  �� �             xz    ��                              ��        �                  ����                            c                         +�                    �           �   l       ��                  f  p  �               h�                    O   ����    e�          O   ����    R�          O   ����    ��      �     m  �z  }          O   n  ��  ��  �z    ��                            ����                                                        �   l       ��                  v  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                                       �   l       ��                 �  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��            0      �  \      �  |      ��                 �  �  �               �                �     �  �       \  �       ��                            7   ����        ��                     �            �                  6   �           ��                    �            �                                                                h  \                                   @            <   L        O   ����  e�          O   ����  R�          O   ����  ��      �  9   �  #   �  �  �   #     �         
                                         "                 
 
 
                     "                                 	 	 	       :   �                   �  �  �      �z      4   �����z      8  �  #     ��                             ��                            ����                                =   �  #                   �           �   l       ��                 �  �  �               L�                    O   ����    e�          O   ����    R�          O   ����    ��      4  �   �  �z            D      �          �  �      ��                  �  �  �              �                �     �  �       p  �       ��                            7   ����    $      ��                     �                              6   �       $ @   ��         4        �                                                                    �z                 �  |           �z           �z                      \   l        O   ����  e�          O   ����  R�          O   ����  ��          �   �  �z          /   �  $                                3   ����P{    ��                              ��        �                   ��                            ����                                                       �   l       ��                 �  B  �               �                    O   ����    e�          O   ����    R�          O   ����    ��        0      �                 �      ��      0         �  @                t�    %  �{  ����       �  �       $  �  \  ���                       d{      %                   � ߱        �  $  �  �  ���                       �{      %                   � ߱            4   �����{      O   �     ��  �{      O   �     ��  |        �  <  �      |      4   ����|                �                      ��                  �  ?                  �                       �  L  �  $  �  �  ���                       0|      %                   � ߱                      �          �  �      ��                   +  �              \]                `	              O       ��          O       e�          O   ����  R�      (         |      8|      4   ����8|                �                      ��                                      ��                           �  $    �  ���                       <|      %                   � ߱                         d|      4   ����d|      O     �� ��      �  <        	     ����   �|     x  �|                                        �|  �       �  �      �|      4   �����|      O      �� ��            !  �  `      �|      4   �����|                p                      ��                  !  *                  �                       !  �  H	    "  �        �|      4   �����|                                      ��                  "  (                  ��                       "  �  �  $  #  D  ���                       }      %                   � ߱          �      D  �                      ��        0         $  &                  ��    %  �}     	     $  p      $  $    ���                       <}      %                   � ߱        �  $  $  p  ���                       l}      %                   � ߱            4   �����}      $  %  �  ���                       �}      %                   � ߱            	  '  8	                                        3   ����$~      O   )  �� ��      
    ,  |	  �	      0~      4   ����0~      O   ,     ��  l~  x~      	               �~      	 A       A       �~      	 @       @           � ߱        4
  V   3  �	  ���                        �    8  P
  �
      �~      4   �����~                �
                      ��                  8  <                  ��                       8  `
  H  /   :                                 3   �����~            8                      3   �����~        ;  d  t      �~      4   �����~      O   ;     ��  �~      F   >       	       ��                                                               %  �          `  x   h                                                                                      (   8   H   X          (   8   H   X    �     %     ��                              ��        �                   ��                            ����                                            ,          �   l       ��                 H  c  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      �'   &    �              �          �'   &                 �          �'   &                            h  /  T  X         '                      3   �����~  �  /  V  �     �  L                      3   ����  �        �                      3   ����X          �                      3   ����d            $  4                  3   ����p      $   V  `  ���                                &                   � ߱        �  �   Z  |        _  �  4      �      4   �����                D                      ��                  _  a                  �                       _  �      $  `  p  ���                       �      &                   � ߱                    &                                               '  (                                 
                        �  & '   ��                            ����                                D/B          c  �   �(  m  �                      
 �                                                                 >  7     �         �(                                    
 �                                                                >  >     �       ��(                                    
 �                                                                >  �    �  
       �(                                    
 �                                                                >  �    �  2       �(                                    
 �                                                                    �     �         �(                                    
 �                                                                >  6    �         �(                                    
 �                                                                >  �    �  
       �(                                    
 �                                                                G  �(    �  P     ��(                                      �                                                                                                                                                                                                           q   d d        ���0��0  � �                                               �                                                                        d     D                                                                 |   s �`                                                              	               �  (  	  4  	  @   x  �� ��                                            
           ,     	             �          :  :       \  �� �p                                                  �(                @      \  �Z�p                                 �                 �(                @      H  � �D/B                                c          �           P ��9�         �              	                              #	       P ��p �2         �                                           9	       P �� �2                                                        Q	        D                                                                    TXS appSrvUtils T-FELogErrores CodCia ErrorDate CodDiv CodDoc NroDoc LogDate LogUser LogEstado CodHash NumDocumento FlagPPLL EstadoPPLL ID_Pos IP_ePos Campo-C tt-w-report-conceptos Tabla de Reportes Task-No Llave-C Campo-D Campo-F Campo-I Llave-I Llave-F Llave-D Campo-L ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST s-codcia s-coddiv s-user-id x-coddiv x-TpoFac x-Moneda REBATE Reporte FchDoc CodCli NomCli DirCli RucCli CodAnt CodPed NroPed NroOrd ImpBrt ImpExo PorIgv ImpIgv ImpDto ImpTot SdoAct FlgEst usuario UsrDscto FlgCie FchCie HorCie CodMon TpoCmb CodAlm LugEnt Tipo CodMov CodVen ImpIsc ImpVta ImpFle FchCan Glosa CodRef NroRef FchVto CodCob CodCta CodAge FlgUbi FlgUbiA FchUbi FchUbiA FlgSit Cndcre ImpInt FmaPgo FchAct FlgSitA TipVta PorDto TpoFac FchCbd NroSal FlgCbd Codope NroMes Nroast FchAnu UsuAnu CodDpto CodProv CodDist FlgCon LugEnt2 FlgAte FchAte imptot2 ImpCto AcuBon NroCard TipBon CCo FlgEnv puntos mrguti Sede Libre_c01 Libre_c02 Libre_c03 Libre_c04 Libre_c05 Libre_d01 Libre_d02 Libre_f01 Libre_f02 FchCre Libre_f03 Libre_f04 Libre_f05 FchCobranza UsrCobranza DivOri ImpPro ImpDto2 GlosaImpDto2 CodCaja Dcto_Otros_Mot Dcto_Otros_Factor Dcto_Otros_VV Dcto_Otros_PV Lista_de_Precios TotalValorVentaNetoOpGravadas TotalValorVentaNetoOpGratuitas TotalTributosOpeGratuitas TotalIGV TotalImpuestos TotalValorVenta TotalPrecioVenta DescuentosGlobales PorcentajeDsctoGlobal MontoBaseDescuentoGlobal TotalValorVentaNetoOpNoGravada TotalDocumentoAnticipo MontoBaseDsctoGlobalAnticipo PorcentajeDsctoGlobalAnticipo TotalDsctoGlobalesAnticipo MontoBaseICBPER TotalMontoICBPER TotalValorVentaNetoOpExoneradas TotalVenta x-Formato 999-999999 N/C pMensaje hProc x-nueva-arimetica-sunat-2021 wWin BUTTON-10 BUTTON-9 RADIO-SET-1 OTROS ADELANTO SELECT-CodDiv uno CcbCDocu CcbTabla Tabla de cobranzas BROWSE-10 Selecciones los documentos presionando Ctrl + Clic simult�neamente x(3) X(12) 99/99/9999 x(50) S/. US$ ->>,>>>,>>9.99 X(10) x(80) fMain N/C por Rebate N/C por A/C N/C por Otros x(8) 20 documentos por vez Seleccione una divisi�n Filtrar: GUI PROBACION O RECHAZO DE NOTAS DE CREDITO - OTROS DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   RADIO-SET-1 SELECT-CodDiv BUTTON-9 BUTTON-10 BROWSE-10 CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE rpta �Procedemos rechazar las notas de cr�dito? ADM-ERROR X �Procedemos con la aprobaci�n?  iStartPage ADM-ERROR ADM-CREATE-OBJECTS k x-Rowid x-docs x-docs-proc x-cuantos x-msg El proceso solo acepta como maximo 20 documentos por cada proceso GENERAL OK LocalCounter ix LocalMensaje oneError GetMessage NO se pudo bloquear la tabla  Ccbcdocu P ERROR-EPOS Se procesaron   documento(s) APROBAR-REBATE DISABLE_UI ENABLE_UI EXITOBJECT EXTORNA-AC FELogErrores GRABA-TEMP-FELOGERRORES GN-DIVI DIVISIONES  -  INITIALIZEOBJECT cMsgs A RECHAZAR-REBATE pCodDoc pNroDoc pRetVal hxProc NO VALIDAR Idx00 Idx01 Idx02 REPO01 REPO02 REPO03 llave00 llave01 llave02 llave03 llave04 llave05 llave06 llave07 llave08 llave09 llave10 llave11 llave12 llave13 llave14 Llave15 Llave16 Codigo Numero Emisi�n Cliente Moneda Importe Total Cuenta Nombre Nombre APROBAR RECHAZAR IDX01 Progress.Lang.SysError Progress.Lang.Error Progress.Lang.Class Progress.Lang.Object �  �4  �  l;      ' �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   �	  �	  �	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc        	  
      '  4  6  H  �     9                                     �  	     :                                       �  L	     ;                                   
    	  �	     <                                   I  T	  �	     =                                   H  I  �	  �	     >                                   T  U  V            
     rpta    �	  H
     ?   �	                              `  b  d  e  g  i             t
     rpta    
  �
  	   @   `
                              s  u  w  x  y  z  {  }  ~  |
        A                                   �  �  �  �
  <     B                                   �  �  �    x     C                                   �  �  �  �  H  �     D                                   �  �  �       E               �                  adm-create-objects  �     !           k   <  !      4     x-Rowid X  "     P     x-docs  x  "     l     x-docs-proc �  "     �     x-cuantos   �  "     �     x-msg   �  !      �     LocalCounter    �  !      �     ix    !           LocalMensaje    4  !      (  *   oneError        !      H  
   hProc   �  �  N   F             �                  Aprobar-Rebate      	  
                  v  x  y  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �        
                   !  "  %  &  '  )  *  +  ,  -  /  3  5  7  P       G               �                  disable_UI  G  H  I  J  �  P     H               D                  enable_UI   Z  \  ^  _  `    �     I               �                  exitObject  m  n  p  d  �     J               �                  Extorna-AC  �  �  4     K                                 Graba-Temp-FeLogErrores �  �  �  �  �  �  �  �  �     L               �                  initializeObject    �  �  �  �  �  �  �  %      �     k   �  %      �     x-Rowid   %      �     LocalCounter       %           cMsgs       %      4     ix  P  x     M   �          h                  Rechazar-Rebate �  �  �                 !  "  #  $  %  &  '  (  )  *  +  ,  3  8  :  ;  <  >  ?  @  B      '       
   hxProc  0  &      (        pCodDoc P  &      H        pNroDoc     &      h        pRetVal 8  �     N   �        �                  validar T  V  Z  _  `  a  c  p  �'       �       �&                      X         T-FELogErrores  �         �         �         �         �      "   �         �                                     (         0         8      "   D         P  
      CodCia  CodDiv  CodDoc  NroDoc  LogDate LogUser CodHash NumDocumento    FlagPPLL    EstadoPPLL  ID_Pos  IP_ePos ErrorDate   LogEstado   Campo-C H  h  �  
   tt-w-report-conceptos   �                                                     (        0         8         @        Task-No Llave-I Llave-C Campo-D Campo-F Campo-I Campo-C Llave-F Llave-D Campo-L     X  `  ~   Reporte H         P         X         `         h         p         x         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                                        (         0         8         @         H         P         X         `         h         p         x         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                              $         ,         4         <         D         L         T         \         d         l         t         |         �         �         �         �  
      �         �  
      �         �         �         �         �         �         �         �         �                                     ,         8         D         L         X         d         p         |         �         �         �         �         �         �         �         �         �         �                  0         P         l         x         �         �         �         �         �         �                   ,          L          l          �          �          �          �          CodCia  CodDoc  NroDoc  FchDoc  CodCli  NomCli  DirCli  RucCli  CodAnt  CodPed  NroPed  NroOrd  ImpBrt  ImpExo  PorIgv  ImpIgv  ImpDto  ImpTot  SdoAct  FlgEst  CodCob  CodCta  usuario FlgCie  FchCie  HorCie  CodMon  TpoCmb  CodAlm  LugEnt  Tipo    CodMov  CodVen  ImpIsc  ImpVta  FchCan  Glosa   CodRef  NroRef  FchVto  CodAge  FlgUbi  FlgUbiA FchUbi  FchUbiA FlgSit  Cndcre  CodDiv  ImpInt  FmaPgo  FchAct  FlgSitA TipVta  PorDto  TpoFac  UsrDscto    FlgCbd  FchCbd  NroSal  Codope  NroMes  Nroast  FchAnu  UsuAnu  CodDpto CodProv CodDist FlgCon  LugEnt2 FlgAte  FchAte  ImpFle  imptot2 ImpCto  AcuBon  NroCard TipBon  CCo FlgEnv  puntos  mrguti  Sede    Libre_c01   Libre_c02   Libre_c03   Libre_c04   Libre_c05   Libre_d01   Libre_d02   Libre_f01   Libre_f02   FchCre  Libre_f03   Libre_f04   Libre_f05   FchCobranza UsrCobranza DivOri  ImpPro  ImpDto2 GlosaImpDto2    CodCaja Dcto_Otros_Mot  Dcto_Otros_Factor   Dcto_Otros_VV   Dcto_Otros_PV   Lista_de_Precios    TotalValorVentaNetoOpGravadas   TotalValorVentaNetoOpGratuitas  TotalTributosOpeGratuitas   TotalIGV    TotalImpuestos  TotalValorVenta TotalPrecioVenta    DescuentosGlobales  PorcentajeDsctoGlobal   MontoBaseDescuentoGlobal    TotalValorVentaNetoOpNoGravada  TotalDocumentoAnticipo  MontoBaseDsctoGlobalAnticipo    PorcentajeDsctoGlobalAnticipo   TotalDsctoGlobalesAnticipo  MontoBaseICBPER TotalMontoICBPER    TotalValorVentaNetoOpExoneradas TotalVenta  �           �   
   appSrvUtils !        !     s-codcia    8!        ,!     s-coddiv    X!        L!     s-user-id   x!       l!     x-coddiv    �!       �!     x-TpoFac    �!       �!     x-Moneda    �!       �!     x-Formato   �!       �!     pMensaje    "       "  
   hProc   H"       ("     x-nueva-arimetica-sunat-2021    d"       \"  
   wWin    �"    	   x"     RADIO-SET-1 �"    
   �"     SELECT-CodDiv   �"        �"  
   gshAstraAppserver   �"        �"  
   gshSessionManager   #        #  
   gshRIManager    D#        0#  
   gshSecurityManager  l#        X#  
   gshProfileManager   �#        �#  
   gshRepositoryManager    �#        �#  
   gshTranslationManager   �#        �#  
   gshWebManager   $        �#     gscSessionId    0$         $     gsdSessionObj   T$        D$  
   gshFinManager   x$        h$  
   gshGenManager   �$        �$  
   gshAgnManager   �$        �$     gsdTempUniqueID �$        �$     gsdUserObj  %        �$     gsdRenderTypeObj    0%        %     gsdSessionScopeObj  L%       D%  
   ghProp  l%       `%  
   ghADMProps  �%       �%  
   ghADMPropsBuf   �%       �%     glADMLoadFromRepos  �%       �%     glADMOk �%       �%  
   ghContainer &       &     cObjectName 0&       (&     iStart  P&       D&     cAppService p&       d&     cASDivision �&       �&     cServerOperatingMode    �&       �&     cFields          �&     iStartPage  �&    X  �&  T-FELogErrores   '    X  '  tt-w-report-conceptos   8'    L  0'  Reporte T'  	 	    H'  CcbCDocu    p'  
 
    d'  CcbTabla    �'   #   �'  FELogErrores         $    �'  GN-DIVI          B   �   �          	    �  �  �  �  �  �  �           �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �   	  	  	  	  v	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  r
  }
  ~
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
                !  "  $  %  &  '  (  )  *  +  -  .  /  0  1  2  4  5  6  7  8  9  :  ;  <  =  >  ?  @  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  �  �                 "  %  /  K  ]  �  �  �  �  =  U  V  p  �  �  �  �  �  �  �  �  �  �  �  u  v  z  �  �           	  "  #  $  &  .  4  :  =  B  F  G  H  J  N  �  �      C  P  ^  q  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �      h� & O:\on_in_co\aplic\lib\lock-wait.i    �+  5� % O:\on_in_co\aplic\lib\lock-genericov3.i  ,  H� $ C:\Progress\OpenEdge\src\adm2\windowmn.i D,  f!  C:\Progress\OpenEdge\src\adm2\containr.i x,  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �,  ��  C:\Progress\OpenEdge\src\adm2\visual.i   �,  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  $-  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    d-  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �-  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �-  Ds   C:\Progress\OpenEdge\gui\fn  .  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   <.  Q.  C:\Progress\OpenEdge\gui\set |.  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �.  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �.  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    /  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  `/  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i �/  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �/  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i 0  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    H0  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �0  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �0  �j  C:\Progress\OpenEdge\gui\get 1  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    ,1  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    p1  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �1  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �1  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i 2  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   \2  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �2  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �2  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  3  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  P3  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i �3  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �3  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   4  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   H4  ��    O:\on_in_co\aplic\CCB\w-aprobacion-nc-otros.w        �  3      �4     �  &   �4  �  �      �4       %   �4  �  �      �4     �  $   5          5  �   �     ,5     �     <5  �   �     L5     �     \5  �   �     l5     S  #   |5  �   =     �5     ;      �5  �   4     �5     2      �5  �   1     �5     /      �5  r        �5  n   �     �5     �  "   6  i   �     6     |     ,6  P   c     <6  �   Z     L6       !   \6  �   �     l6     �     |6  �   �     �6     �     �6  �   �     �6     �     �6  g   z     �6     [     �6  O   C     �6  �   �     �6     �      7  �   �     7     C     ,7  �   8     <7          L7  �        \7     �     l7  �   �     |7     �     �7  �   �     �7     �     �7  �   �     �7     z     �7  �   w     �7     U     �7  }   I     �7     '     8     �     8     ]     ,8          <8  7   �     L8  �   �     \8  O   �     l8     �     |8     ]     �8  �        �8  �        �8  O   �
     �8     �
     �8     �
     �8  �   z
     �8  x   r
  
   �8  M   ]
     9     L
     9      
     ,9  a   �	  
   <9  �  �	     L9     �	     \9  �  v	     l9  O   h	     |9     W	     �9     		     �9  �   3     �9          �9     Z     �9  x   T     �9     ;     �9     �     �9     �     :     �     :     �     ,:  Q   �  
   <:     '     L:     �  
   \:     �     l:     �  
   |:  f   �     �:     7  	   �:  "   �     �:     �     �:     �     �:  Z   m     �:     u     �:     6     �:     "     ;          ;     �     ,;  2   �       <;     K      L;     !       \;           