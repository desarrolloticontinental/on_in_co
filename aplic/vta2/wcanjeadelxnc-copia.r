	��V�:�a|6  ��              g                                �� 367C0111utf-8 MAIN d:\newsie\on_in_co\APLIC\vta2\wcanjeadelxnc-copia.w,, PROCEDURE proc_AplicaDoc-2,,INPUT para_CodDoc CHARACTER,INPUT para_NroDoc CHARACTER,INPUT para_NroDocCja CHARACTER,INPUT para_TpoCmb DECIMAL,INPUT para_ImpNac DECIMAL,INPUT para_ImpUSA DECIMAL PROCEDURE initializeObject,, PROCEDURE Genera-NC,, PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE _corre-program,, PROCEDURE _busca-lookup,,INPUT campo_name CHARACTER,INPUT program_call CHARACTER,OUTPUT program_name CHARACTER PROCEDURE adm-imprime,, PROCEDURE adm-busca,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION fMoneda,character,INPUT iCodMon INTEGER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER         �              �             :�  �  ��              H�              p?    +   ,- `     �. `     �/ �  	   �5 l  
   D= �  A   �A `  B   DE �   R   8F |  S   �G �  T   �M $  U   �N �"  V   Lq X  W   �y �  X           <� �  ̆ �  ? �� �*  iSO8859-1                                                                           ��   1 �           $:                         �     
             ��                ,�  Hl    |l  
    L�  P�         ��  �   Ȭ      Ԭ          �                                             PROGRESS                         �;           
    
                                   �                                                                                                     
                                INTEGRAL                         PROGRESS                         �        �         �C                      �a�a               �     �  80                  �  �                        �  ~~     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2CODCAJADCTO_OTROS_MOTDCTO_OTROS_FACTORDCTO_OTROS_VVDCTO_OTROS_PVLISTA_DE_PRECIOSTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �        �      %    C                      �a�a            %   �                              �  x                        �  ~~     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2CODCAJADCTO_OTROS_MOTDCTO_OTROS_FACTORDCTO_OTROS_VVDCTO_OTROS_PVLISTA_DE_PRECIOSTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �"        �      0   C                      �a�a            0   �                              �  p                         �  ~~     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2CODCAJADCTO_OTROS_MOTDCTO_OTROS_FACTORDCTO_OTROS_VVDCTO_OTROS_PVLISTA_DE_PRECIOSTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    P#         �       5   �  4�  �   ��  5   Qb  �a�a��  ~       5              hn          Tx      �   �#             �#                                                                                          �             H$             4$                                                                                          �             �$             �$                                                                                          �             @%  	           ,%                                                                                          �             �:        �                                �a�a               �                              �  �%                      P*  �%  ~~     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2CODCAJADCTO_OTROS_MOTDCTO_OTROS_FACTORDCTO_OTROS_VVDCTO_OTROS_PVLISTA_DE_PRECIOSTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                                                                                                                                                       	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                  :                  ;                  <                  =                  >                  ?                  @                  A                  B                  C                  D                  E                  F                  G                  H                  I                  J                  K              
   L                  M              
   N                  O                  P                  Q                  R                  S                  T                  U                  V                  W                  X                  Y                  Z                  [                  \                  ]                  ^                  _                  `                  a                  b                  c                  d                  e                  f                  g                  h                  i                  j                  k                  l                  m                  n                  o                  p                  q                  r                  s                  t                  u                  v                  w                  x                  y                  z                  {                  |                  }                  ~                                                   8:  2            L:  >            `:  J            t:  V            �:  c                p            �;     �	  �      �	                         �ɺ[            �	  b|                              �  ;                      X;  ,;  ,      PROGRAMA_LOOKUPPROGRAMA_CALLCAMPODESCRIPCION                                        4<  �      �;  
    
                  �;  d<              <                                                                                          �          
  �<  �      \<  
    
                  H<  =             �<                                                                                          �          
  �=  �      =  
    
                  �<  �=             x=                                                                                          �          
  8>  �      �=  
    
                  �=  h>             $>                                                                                          �          
  �>  �      `>  
    
                  L>  ?             �>                                                                                          �          
  �?  �      ?  
    
                  �>  �?             |?                                                                                          �          
  <@  �      �?  
    
                  �?  l@             (@                                                                                          �          
  �@        d@  
    
                  P@  A             �@                                                                                                    
  �A        A                         �@  �A             �A                                                                                                      @B        �A                        �A  pB             ,B                                                                                                      �B  +      hB  
    
                  TB  C             �B                                                                                          +          
  �C  9      C  
    
                   C  �C             �C                                                                                          9          
  DD  G      �C  
    
                  �C  tD             0D                                                                                          G          
  �D  U      lD                        XD   E             �D                                                                                          U            �E  e      E                        E  �E             �E                                                                                          e            HF  p      �E                        �E  xF             4F                                                                                          p                �      pF                        \F  �F             �F                                                                                          �            �H  #   &  �      &                         �ɺ[            "&  �1                              �  tG                      H  �G  ~      CODCIACODDOCNROSERCORRELATIVOCODALMLISTAPRECIOCODDIVPRINTERCODPRONROIMPFCHIMPNROININROFINCODMOVTIPMOVFLGESTFLGCICID_POSID_POS2                                                                        	          
                                                                                                              �O  $   H&  �      H&                         �\            P&  '�                              �  hI                      �K  xI  RT     CODCLINOMCLIDIRCLIRUCTPOCLILOCCLIFAXCLIFCHINGFLGSITCODPOSCODCIAREFERENCIASCODPAISCODDEPTCODPROVCODDISTGIRCLICNDVTATELFNOSE-MAILFCHACTCLFCLIUSUARIOREPLEGFNREPRTRANSPORTEMONLCIMPLCCODVENDIRENTFCHCESCONTACFCHVLCCODANTUSRLCVENANTFLAGAUTUSRAUTOBSAUTCODDIVDEPENTPROENTDISENTLINCREUSRCRETOTUSOFCHAUTAVAL1AVAL2TLFLOGJFELOGDNIRUCOLDCANALCLFCOMNROCARDDIRREFCODUNICOAPEPATAPEMATNOMBRECOB_DIASCOB_HORARIOCOB_DIRECCIONCOB_CARTACOB_GLOSACODIBCCLFCLI2LIBRE_C01LIBRE_C02LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02FLAGTIPOFLAGUSUARIOFLAGMIGRACIONFLAGFECHACM_CLFCLI_PCM_CLFCLI_TSWBAJASUNATSWCARGASUNAT                                                                        	          
                                                                                                                                                                                                                                    !          "          #          $          %          &          '          (          )          *          +          ,          -         .         /          0         1         2         3          4         5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          Q          R          S          T          U          V          �R  %   �&  �      �&                         �ɺ[            �&  �l                              �  <P                      LQ  LP  �       CODCIACLVMODPORIGVDIAS-RESHORA-RESMINU-RESBRRPEDCLIVARTPOCMBITEMS_FACTURAITEMS_BOLETAITEMS_GUIASITEMS_PEDIDOITEMS_N_CREDITOITEMS_N_DEBITODTOMAXDTODISDTOMAYMRGPUBDTOPROITEMS_PEDMOSCLA_COMPRACLA_VENTACODCTAMRGMINMRGMAYMRGDISFACPORALMALTTOLVENROUNDOCUPORMORA                                                                       	          
                                                                                                                                                                                                                                     !          �S  (   S'  �      S'                         �ɺ[            S'  I                              �  LS                      tS  \S        FECHACOMPRAVENTACODDIV                                          V  )   >(  �      >(                         �ɺ[            E(  ��                              �  $T                      �T  4T  �      CODCIACODVENNOMVENCOMPORCODANTPTOVTACCOFLGESTLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_F03LIBRE_F04LIBRE_F05FLAGTIPOFLAGUSUARIOFLAGMIGRACIONFLAGFECHA                                                                        	          
                                                                                                                                                                �\  *   P(  �      P(                        �a�a            P(  ��                              �  �V                      �Y  �V  8A     CODCIACODDOCNRODOCNROITMUNDVTACODMATPREUNIPORDTOIMPDTOIMPLINCANDESAFTIGVAFTISCPREBASPREVTAIMPIGVIMPISCFACTORCANDEVPORDTO2PESMATCODCLIALMDESPOR_DSCTOSFLG_FACTORFCHDOCCODDIVIMPCTOPUNTOSMRGUTIIMPPROIMPDTO2PORDCTO_ADELANTOIMPDCTO_ADELANTODCTO_OTROS_MOTDCTO_OTROS_FACTORDCTO_OTROS_VVDCTO_OTROS_PVCTIPOAFECTACIONCPREUNISINIMPUESTOFACTORDESCUENTOTASAIGVIMPORTEUNITARIOSINIMPUESTOIMPORTEREFERENCIALIMPORTEBASEDESCUENTOIMPORTEDESCUENTOIMPORTETOTALSINIMPUESTOMONTOBASEIGVIMPORTEIGVIMPORTETOTALIMPUESTOSIMPORTEUNITARIOCONIMPUESTOCIMPORTEVENTAEXONERADOCIMPORTEVENTAGRATUITOCSUMAIMPTETOTALSINIMPUESTOCMONTOBASEIGVCSUMAIGVCOTROSTRIBUTOSOPGRATUITOIMPUESTOBOLSAPLASTICOMONTOTRIBUTOBOLSAPLASTICOCANTIDADBOLSAPLASTICOMONTOUNITARIOBOLSAPLASTICOCIMPORTETOTALCONIMPUESTOIMPORTEBASEDESCUENTONOAFECTOFACTORDESCUENTONOAFECTOIMPORTEDESCUENTONOAFECTO                                                                      	          
                                                                                                                                                                                                                                     !          "         #         $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          �^  +   _(  �      _(                         �ɺ[            h(  ��                              �  l]                      �]  |]  t      CODIGONOMBRECODCTAAFECTOCODCIATABLALIBRE_C01LIBRE_C02LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02RESERVADO                                                                      	          
                                                                      �i  .      �       )   C                      �a�a             )  �  X                           �  $_                      �c  4_  ~~     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2CODCAJADCTO_OTROS_MOTDCTO_OTROS_FACTORDCTO_OTROS_VVDCTO_OTROS_PVLISTA_DE_PRECIOSTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                        /    )  �       )                        �ɺ[             )  Vw                              �  j                      �j  ,j  �      CODCIACODDIVNRODOCFCHMOVHRAMOVUSUARIOIMPTOTCODDOCCODCLICODMONCODBCOCODREFNROREFTPOCMBFCHDOCCHR__01CHR__02CHR__03CHR__04CHR__05DEC__01DEC__02DEC__03DEC__04DEC__05DATE__01DATE__02LOG__01LOG__02                                                                       	          
                                                                                                                                                                                                                               " ��                                             
  �          �m  n  L l�l            
                                                                     
             
             
                                         
                                                                                                                L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                                                                                                                                                                                                          9                                                                                                                                                                   !                  "                  #                  $                  I                  %                  &                  '                  (                  )                                                      *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  ;                  <                  :                  =                  >                  ?                  @                  A                  B                  C                  D                  E                  F                  G                  H                  J                  K              
   L                  M              
   N                  O                  P                  Q                  R                  S                  T                  U                  V                  W                  X                  Y                  Z                  [                  \                  ]                  ^                  _                  `                  a                  b                  c                  d                  e                  f                  g                  h                  i                  j                  k                  l                  m                  n                  o                  p                  q                  r                  s                  t                  u                  v                  w                  x                  y                  z                  {                  |                  }                  ~                                                   (�  0�  4�  <�  8�          @�             T�  \�  d�  t�  l�                         x�  ��  ��  ��  ��                         ��  ��  ��  ��  ��                         Ȏ  Ў  ؎  �  ��                         �  �  ��  �  �                         �  �   �  8�  ,�                          <�  D�  L�  T�  P�                          X�  `�  h�  ��  x�                          ��  ��  ��  ��                             ��  ��  ��  ȏ  ��                         ̏  ԏ  ܏  ��  �                           �  �  �  8�  (�                         <�  D�  T�  |�  h�                         ��  ��  ��  ��  ��                         ��  ��  Đ  �  Ԑ                         �  �   �  (�  �                         ,�  4�  D�  d�  T�                         h�  p�  ��  ��  ��                         ��  ��  ��  ��  ��                         đ  ̑  ԑ  �  ܑ                          �  ��  ��  �  �                           �  (�  ,�  4�                              8�  @�  L�  \�  T�                          `�  h�  p�  ��  ��                          ��  ��  ��  ��  ��                          ��  ��  Ȓ  �  ؒ                         �  �  ��  �   �                         �  �  �  D�  0�                          H�  P�  X�  ��  l�                          ��  ��  ��  ��  ��          ��              ԓ  ܓ  �  ��  �                           �  �  �  0�  $�                         4�  <�  L�  d�  X�                         h�  p�  ��  ��  ��                         ��  ��  ��  �  Д                          �  ��  ��  �  �                           �  (�  0�  @�  8�                         D�  L�  T�  d�  \�                         h�  p�  |�  ��  ��                         ��  ��  ��  ؕ  ̕                          ܕ  �  �  �  ��                         �  �  �  (�   �                          ,�  4�  8�  P�  D�                          T�  \�  `�  x�  l�                          |�  ��  ��  ��  ��                          ��  ��  Ė  ��  ؖ                          �  �  �  �  ��                          �  �  �  <�  0�                          @�  H�  P�  `�  X�                         h�  p�  ��  ��  ��                         ��  ��  ��  ԗ  ��                          ؗ  ��  �  ��                              ��   �  �  �                              �  $�  ,�  D�  8�                          H�  P�  X�  p�  d�                         t�  |�  ��  ��  ��                         ��  ��  ��  ��  ��                          ��  Ș  И  �  ��                          ��  ��  �  �                              �  �  �  ,�  (�          0�              P�  X�  \�  d�  `�          h�              ��  ��  ��  ��  ��          ��              ș  Й  ܙ  ��  �                          ��   �  �  �  �                          �  $�  ,�  L�  <�                          P�  X�  `�  x�  l�                          |�  ��  ��  ��  ��                          ��  ��  ��  ؚ  Ț                         ܚ  �  �  �   �                          �   �  $�  4�  ,�                         8�  @�  L�  \�  T�          `�              ��  ��  ��  ��                             ��  ��  ��  ̛  ě                         Л  ؛  �  ��  �                         ��  �  �  �  �                           �  (�  ,�  <�  4�                          @�  D�  L�  l�  \�          p�              ��  ��  ��  ��                              ��  ��  Ĝ  ̜                             М  ؜  �  �                             �  ��   �  �                              �  �   �  ,�                              0�  <�  D�  P�                              T�  `�  h�  t�                              x�  ��  ��  ��                              ��  ��  ��  ��                              ��  ̝  ��  �                             �  ��  �  �                              �  ,�  8�  D�                              H�  T�  `�  l�                              p�  x�  ��  ��                              ��  ��  ��  ��                              ��  Ğ  О  ܞ                              ��  �  ��  �                              �  �   �  0�                              4�  @�  H�  \�                              `�  h�  p�  ��  x�                         ��  ��  ��  ��                             ��  ��  ��  ȟ                             ̟  ܟ  �  ��                              ��   �  �   �  �                          $�  4�  <�  L�                              P�  d�  t�  ��                             ��  ��  ��  ��                             Ġ  Ԡ  �  ��                             ��  �  �  ,�                              0�  P�  `�  ��                             ��  ��  ��  ԡ                             ء  ��  �   �                             $�  0�  @�  L�                             P�  `�  p�  ��                             ��  ��  ��  ��                             ��  ̢  ܢ  �                             ��  �  �  ,�                             0�  H�  T�  l�                             p�  ��  ��  ��                             ��  ܣ  �  �                             �  (�  8�  P�                             T�  t�  ��  ��                             ��  Ȥ  Ԥ  ��                             ��  �  $�  @�                             D�  T�  d�  t�                             x�  ��  ��  ��                             ��  ԥ  �  �                             �  �  $�  0�                                                                         CodCia  999 Cia Cia 0   C�digo de compa�ia  CodDoc  x(3)    Codigo  Codigo      NroDoc  X(12)   Numero  Numero      FchDoc  99/99/9999  Fecha   Fecha   TODAY   CodCli  x(11)   Cliente Cliente     NomCli  x(50)   Nombre  Nombre      DirCli  x(60)   Direccion   Direccion       RucCli  x(11)   Ruc Ruc     CodAnt  X(10)   Codigo Anterior Codigo!Anterior     CodPed  x(10)   CodPed      NroPed  X(12)   Pedido  Pedido      NroOrd  x(12)   Orden de Compra Orden de!Compra     ImpBrt  ->>,>>>,>>9.99  Importe Bruto   Importe Bruto   0   ImpExo  ->>,>>>,>>9.99  Importe Exonerado   Importe Exonerado   0   PorIgv  ->>9.99 % I.G.V.    % I.G.V.    0   ImpIgv  ->>,>>>,>>9.99  Importe I.G.V.  Importe I.G.V.  0   ImpDto  ->>,>>>,>>9.99  Importe Descuento   Importe Descuento   0   ImpTot  ->>,>>>,>>9.99  Importe Total   Importe Total   0   SdoAct  ->>,>>>,>>9.99  Importe Total   Importe Total   0   FlgEst  X   Estado  Estado  P   usuario x(10)   usuario usuario     UsrDscto    X(10)   Resp.!Dscto.    Resp.!Dscto.        FlgCie  x   FlgCie  P   FchCie  99/99/9999  Cierre  Cierre  ?   HorCie  x(5)    Hora de cierre  Hora de!cierre      CodMon  9   Moneda  Moneda  1   TpoCmb  Z,ZZ9.9999  Tipo de cambio  T/Cambio    0   CodAlm  x(3)    Almacen Almacen     LugEnt  x(60)   Lugar de entrega    Lugar de entrega        Tipo    x(20)   Tipo de documento   Tipo de documento       CodMov  99  C�digo de movimiento    C�digo!movimto. 0   C�digo de movimiento    CodVen  x(10)   Vendedor    Vendedor        ImpIsc  ->>,>>>,>>9.99  Importe Isc Importe Isc 0   ImpVta  ->>,>>>,>>9.99  Valor Venta Valor venta 0   ImpFle  ->>,>>>,>>9.99  Importe Flete   Importe Flete   0   FchCan  99/99/9999  Fecha de cancelacion    Fecha de!cancelacion    ?   Glosa   x(60)   Observaciones   Observaciones       CodRef  x(3)    Codigo  Codigo      NroRef  X(12)   Numero  Numero      FchVto  99/99/9999  Fecha de vencimiento    Fecha de!Vencimiento    ?   CodCob  X(10)   Cobrador    Cobrador        CodCta  X(10)   Cuenta Contable Cuenta      CodAge  X(10)   Agencia Agencia     FlgUbi  X   Ubicaci�n   Ubicaci�n       FlgUbiA X   Ubicaci�n   Ubicaci�n       FchUbi  99/99/9999  Fecha de Ubicaci�n  Fecha   ?   FchUbiA 99/99/9999  Fecha de Ubicaci�n  Fecha   ?   FlgSit  X   Situaci�n   Situaci�n       Cndcre  X   Condicion de Credito    Condicion       CodDiv  x(5)    C.Div   C.Div   00000   ImpInt  ->>,>>>,>>9.99  Intereses   Intereses   0   FmaPgo  X(8)    Condicion de ventas Condicion de!venta      FchAct  99/99/9999  FchAct  ?   FlgSitA X   Situacion Anterior      TipVta  X(1)    Tipo Venta  Tipo venta      PorDto  >>9.99  % Dscto.    % Dscto.    0   TpoFac  X(1)    Tipo    Tipo        FchCbd  99/99/9999  Fecha   Fecha   ?   NroSal  X(12)   Numero Salida   Numero!Salida       FlgCbd  yes/no  FlgCbd  no  Codope  xxx Operacion   Ope     Ingrese la Operacion Contable   NroMes  99  Mes Mes 0   Ingrese el mes de trabajo   Nroast  x(6)    Asiento Comprobte       Ingrese el Nro. de Asiento  FchAnu  99/99/99    Fecha   Fecha!Anulacion ?   UsuAnu  X(10)   Usuario Usuario     CodDpto X(2)    Departamento    Departamento        CodProv X(2)    Provincia   Provincia       CodDist X(2)    Distrito    Distrito        FlgCon  x(1)    Flag Control    Flag!Control        LugEnt2 x(60)   Lugar de entrega    Lugar de entrega        FlgAte  X   Estado  Estado      FchAte  99/99/9999  Fecha   Fecha   ?   Fecha de Despacho de Almacen    imptot2 ->>>,>>>,>>9.99 imptot2 0   ImpCto  ->>,>>>,>>9.99  ImpCto  ImpCto  0   AcuBon  ->>>,>>>,>>9.99 AcuBon  AcuBon  0   NroCard x(8)    NroCard Nrocard     TipBon  99  TipBon  TipBon  0   CCo X(5)    Centro de Costo Centro!de Costo     Centro de Costo FlgEnv  Si/No   El pedido es para enviar?   No  puntos  ->>,>>9.99  puntos  0   mrguti  ->>,>>9.99  mrguti  0   Sede    x(5)    Sede        Libre_c01   x(60)   Libre_c01       Libre_c02   x(60)   Libre_c02       Libre_c03   x(60)   Libre_c03       Libre_c04   x(60)   Libre_c04       Libre_c05   x(60)   Libre_c05       Libre_d01   ->>>,>>>,>>9.99<<<  Libre_d01   0   Libre_d02   ->>>,>>>,>>9.99<<<  Libre_d02   0   Libre_f01   99/99/99    Libre_f01   ?   Libre_f02   99/99/99    Libre_f02   ?   FchCre  99/99/99    FchCre  ?   Libre_f03   99/99/99    Libre_f03   ?   Libre_f04   99/99/99    Libre_f04   ?   Libre_f05   99/99/99    Libre_f05   ?   FchCobranza 99/99/9999  Fecha Cobranza  ?   UsrCobranza x(10)   Usuario Cobranza        DivOri  x(5)    Origen  Origen      ImpPro  ->>>,>>>,>>9.99 ImpPro  0   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   GlosaImpDto2    x(30)   GlosaImpDto2        CodCaja X(10)   Codigo Caja Codigo!Caja     Dcto_Otros_Mot  x(20)   Dcto_Otros_Mot      Dcto_Otros_Factor   ->>,>>9.999999  Dcto_Otros_Factor   0   Dcto_Otros_VV   ->>>,>>>,>>9.999999 Dcto_Otros_VV   0   Dcto_Otros_PV   ->>>,>>>,>>9.999999 Dcto_Otros_PV   0   Lista_de_Precios    x(8)    Lista_de_Precios        TotalValorVentaNetoOpGravadas   >>>>>>>>>>>9.99 TotalValorVentaNetoOpGravadas   0   TotalValorVentaNetoOpGratuitas  >>>>>>>>>>>9.99 TotalValorVentaNetoOpGratuitas  0   TotalTributosOpeGratuitas   >>>>>>>>>>>9.99 TotalTributosOpeGratuitas   0   TotalIGV    >>>>>>>>>>>9.99 TotalIGV    0   TotalImpuestos  >>>>>>>>>>>9.99 TotalImpuestos  0   TotalValorVenta >>>>>>>>>>>9.99 TotalValorVenta 0   TotalPrecioVenta    >>>>>>>>>>>9.99 TotalPrecioVenta    0   DescuentosGlobales  >>>>>>>>>>>9.99 DescuentosGlobales  0   PorcentajeDsctoGlobal   >>9.99999   PorcentajeDsctoGlobal   0   MontoBaseDescuentoGlobal    >>>>>>>>>>>9.99 MontoBaseDescuentoGlobal    0   TotalValorVentaNetoOpNoGravada  >>>>>>>>>>>9.99 TotalValorVentaNetoOpNoGravada  0   TotalDocumentoAnticipo  >>>>>>>>>>>9.99 TotalDocumentoAnticipo  0   MontoBaseDsctoGlobalAnticipo    >>>>>>>>>>>9.99 MontoBaseDsctoGlobalAnticipo    0   PorcentajeDsctoGlobalAnticipo   >>9.99999   PorcentajeDsctoGlobalAnticipo   0   TotalDsctoGlobalesAnticipo  >>>>>>>>>>>9.99 TotalDsctoGlobalesAnticipo  0   MontoBaseICBPER >>>>>>>>>>>9.99 MontoBaseICBPER 0   TotalMontoICBPER    >>>>>>>>>>>9.99 TotalMontoICBPER    0   TotalValorVentaNetoOpExoneradas >>>>>>>>>>>9.99 TotalValorVentaNetoOpExoneradas 0   TotalVenta  >>>>>>>>>>>9.99 TotalVenta  0   �   2 B W � � ��  ���������   �               P   P�         �   �   ��  00000  �      �    �       �   �           � �           �            �������                                    b)        j)        r)        z)        �)        �)        �)        �)        �)        �)        �)        �)        �)        �)        �)        �)        �)                �     i  i  i  i      i  i  i      i  i  i  i  i  i      i  i  i  i      i  i  i  i  i      i  i  i  i  i  j      i  i  i  i  i      i 	 i 
 i  i  i      i  i  i  i  i      i  i  i  i  i  i      i  i  i  i  i      i  i  i  i  i  i      i  i  i      i  i  i  i  i      i  i  i  i  i  i      i  i  i  i      i  i  i  i  i     	1 	 	 	 	) 	8 	 	 	' 	( 	 	E 	 	G 	 	 	 	c 	    :   A   H   O   V   ]   d   k   r   y   �   �   �   �   �   �   �   �   �   �   R  Y  �   �   �   �   �   �   �             "  0  7  =  D  K  `  g  n  v  }  �  �  �  �  �  �  �  �  �  �  �   �  �  �  �  �  �  �  �          #  +  2  )  9  A  H  O  W  ^  b  i  p  w  |  �  �  �  �  �  �  �  �  �  �  �  �  �        !  )  6  >  M  _  m  {  �  �  �  �  �  �      /  E  ^  }  �  �  �  �  �    +    ��                                                                              �          ����                            \    4�                   ��    �*   ��    �)   ɫ    j)   �"    �*  # �     �*  $ �    �*  % �x    j)  ( �u    j)  ) 1�    j)  + �c    r)  # ��    undefined                                                               �       8�  �   l   H�    ��                  �����               ��                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       �     D          assignFocusedWidget         �      �     6      LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    J      LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    \      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          r      LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    ~      LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    �      LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �      LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |          LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �          LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    $      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 1      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    <      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    I      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    ]      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    k      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    {      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    �      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    fMoneda     u   ����  �             �                      �                     �                     �       )       )       �                         �                    �                        � ߱            V   �����
   �x
                        �    �  �  �  x  P      4   ����P      o   �  
     �                              �  x  NA  �  �  �  �  �     �     �    �    �        $    8  `  L  
`  `  $  t    �     �      $  �  �  ���                       �     
 
                   � ߱        �                         � ߱        (  $  �  �  ���                       �  o   �      \      �                         �     �  �  �  �    �G     �  ,     @     T                  P                   ��                       8              �]�                    O   ����    e�          O   ����    R�          O   ����    ��      �  /     |                                 3   ����h          �     �    ��                            ����                                        �                    �                      g                                               �          �  �      ��                      �              dɒ                    O   ����    e�          O   ����    R�          O   ����    ��              �     �    ��                            ����                                        @                                          g                                 L       "�                  �                         � ߱        $  $  	  �  ���                       4  g   d  <         g �                            L          �  �      ��                  d  h  �              �ɒ                    O   ����    e�          O   ����    R�          O   ����    ��        @         �          D  @         0              � ߱            $   e    ���                         ��                              ��        �                  ����                                        P                    x                      g                               p  g   j  L          g4                                     �  �      ��                 j  p  �              lʒ                    O   ����    e�          O   ����    R�          O   ����    ��      X    k  0  @      X      4   ����X      O   k  ��  ��  �        l  t  �      �      4   �����                                       ��                  l  o                  ��                       l  �  �  /   m  ,     <                          3   �����  l        \                      3   �����            �                      3   ����        n    }        ��                              ��        �                  ����                                        `                    �                      g                                 g   r  �         g��            g4�                           d          4        ��                 r  v  L              <�                    O   ����    e�          O   ����    R�          O   ����    ��            s  �  �             4   ����                                       ��                  s  u                  ��                       s  �        t  (  8      T      4   ����T        t  �     �    ��                              ��        �                  ����                                        �                    P                      g                               `#  g   �  $         g!\!                           �          �  �      ��                 �  �  �              �\�                    O   ����    e�          O   ����    R�          O   ����    ��      0    �          �      4   �����      O   �  ��  ��  �        �  L  �  L   �      4   �����                                       ��                  �  �                  4]�                       �  \       
                0     
                    � ߱        L  $  �  �  ���                       p  /   �  x     �                          3   ����D  �        �                      3   ����`  �        �                      3   ����t                                3   �����      $   �  D  ���                                                   � ߱        �    �  �        �      4   �����                `                      ��                  �  �                  �]�                       �  �  �  @         �             @         �              � ߱        �  $   �    ���                           p   �     �  |  �    �     4  @  L                         � ߱            $  �  �  ���                           P     X  d                         � ߱            $  �  $  ���                           O   �  ��  ��  p        �  �  �  �  �      4   �����  �  @         �              � ߱            $   �  �  ���                         @         �          @  @         ,          �  @         |          �  @         �          �  @         �              � ߱            $   �    ���                                     \                       ��                  �  �                  �t�                       �  �        �  x   �       	      4   ����	  �	  @         t	          �	  @         �	              � ߱            $   �  �   ���                         ��                              ��        �                  ����                                        8                    �                       g                               adm-busca       �!                                                           X	  	                   adm-imprime �!   "                                                           k	                     _busca-lookup   ,"  �"  �       h         	     �                          �  �	                     _corre-program  �"  �"              �     
     ,                          (  �	                     ��      |#  �#            4   ����                $                      ��                    &                  ���                         �#  �$      $$  4$      D      4   ����D      $     `$  ���                       �  @         �              � ߱              #  �$  �$      �      4   �����      $  $  �$  ���                       ,  @                       � ߱        assignPageProperty                              �%  �%      ��                  �  �  �%              �t�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   &             �%               ��                   &           ��                            ����                            changePage                              �&  �&      ��                  �  �  '              X��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �'  �'      ��                  �  �  (              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  ((           ��                            ����                            constructObject                             $)  )      ��                  �  �  <)              � �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �)             T)               �� 
  �)             |)  
             ��   �)             �)               �� 
                 �)  
         ��                            ����                            createObjects                               �*  �*      ��                  �  �  �*              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �+  �+      ��                  �  �  �+              䭔                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �+           ��                            ����                            destroyObject                               �,  �,      ��                  �  �  -              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �-  �-      ��                  �  �  .              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  $.           ��                            ����                            initializeObject                                $/  /      ��                  �  �  </              x͔                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               40  0      ��                  �  �  L0              (Δ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               41  1      ��                  �  �  L1              hْ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d1           ��                            ����                            notifyPage                              \2  D2      ��                  �  �  t2              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �2           ��                            ����                            passThrough                             �3  l3      ��                  �  �  �3              �^�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �3             �3               ��                  �3           ��                            ����                            removePageNTarget                               �4  �4      ��                  �  �  �4              �7�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  @5             5  
             ��                  45           ��                            ����                            selectPage                              ,6  6      ��                  �  �  D6              �O�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  \6           ��                            ����                            toolbar                             P7  87      ��                  �  �  h7              0A�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �7           ��                            ����                            viewObject                              x8  `8      ��                  �  �  �8              �g�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                x9  `9      ��                  �  �  �9              �f�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �9           ��                            ����                            disablePagesInFolder    
      :      H:    �	      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder (:      t:      �:    �	      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  �:      �:      ;    
      HANDLE, getCallerWindow �:      ;      @;    
      HANDLE, getContainerMode     ;      H;      |;    '
      CHARACTER,  getContainerTarget  \;      �;      �;    8
      CHARACTER,  getContainerTargetEvents    �;      �;      <    K
      CHARACTER,  getCurrentPage  �;      <      @<     d
      INTEGER,    getDisabledAddModeTabs   <      L<      �<  !  s
      CHARACTER,  getDynamicSDOProcedure  d<      �<      �<  "  �
      CHARACTER,  getFilterSource �<      �<      =  #  �
      HANDLE, getMultiInstanceActivated   �<      =      H=  $  �
      LOGICAL,    getMultiInstanceSupported   (=      T=      �=  %  �
      LOGICAL,    getNavigationSource p=      �=      �=  &  �
      CHARACTER,  getNavigationSourceEvents   �=      �=      >  '  �
      CHARACTER,  getNavigationTarget �=      $>      X>  (        HANDLE, getOutMessageTarget 8>      `>      �>  )  '      HANDLE, getPageNTarget  t>      �>      �>  *  ;      CHARACTER,  getPageSource   �>      �>      ?  +  J      HANDLE, getPrimarySdoTarget �>      ?      D?  ,  X      HANDLE, getReEnableDataLinks    $?      L?      �?  -  l      CHARACTER,  getRunDOOptions d?      �?      �?  .  �      CHARACTER,  getRunMultiple  �?      �?      �?  /  �      LOGICAL,    getSavedContainerMode   �?      @      @@  0  �      CHARACTER,  getSdoForeignFields  @      L@      �@  1  �      CHARACTER,  getTopOnly  `@      �@      �@  2 
 �      LOGICAL,    getUpdateSource �@      �@      �@  3  �      CHARACTER,  getUpdateTarget �@       A      0A  4  �      CHARACTER,  getWaitForObject    A      <A      pA  5  �      HANDLE, getWindowTitleViewer    PA      xA      �A  6        HANDLE, getStatusArea   �A      �A      �A  7        LOGICAL,    pageNTargets    �A      �A      $B  8  )      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject B      \B      �B  9  6      LOGICAL,INPUT h HANDLE  setCallerProcedure  lB      �B      �B  :  F      LOGICAL,INPUT h HANDLE  setCallerWindow �B      �B       C  ;  Y      LOGICAL,INPUT h HANDLE  setContainerMode     C      8C      lC  <  i      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  LC      �C      �C  =  z      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �C      �C      D  >  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �C      8D      pD  ?  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  PD      �D      �D  @  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �D      �D      (E  A  �      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  E      HE      |E  B  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   \E      �E      �E  C  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �E      F      DF  D        LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource $F      tF      �F  E  !      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �F      �F      G  F  5      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �F      ,G      `G  G  O      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget @G      �G      �G  H  c      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �G      �G      H  I  w      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �G      (H      XH  J  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget 8H      xH      �H  K  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �H      �H      I  L  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �H      8I      hI  M  �      LOGICAL,INPUT phObject HANDLE   setRunDOOptions HI      �I      �I  N  �      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �I      �I      J  O  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �I      0J      hJ  P  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields HJ      �J      �J  Q        LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �J      �J       K  R 
       LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource  K      @K      pK  S  !      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget PK      �K      �K  T  1      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    �K      �K      L  U  A      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �K      <L      tL  V  R      LOGICAL,INPUT phViewer HANDLE   getObjectType   TL      �L      �L  W  g      CHARACTER,  setStatusArea   �L      �L       M  X  u      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �M  �M      ��                  f  g  �M              лt                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �N  �N      ��                  i  j  �N              l�t                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �O  �O      ��                  l  m  �O              �r                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �P  �P      ��                  o  p  �P              ��r                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �Q  �Q      ��                  r  t  �Q              ��r                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �Q           ��                            ����                            getAllFieldHandles  �L      `R      �R  Y  �      CHARACTER,  getAllFieldNames    tR      �R      �R  Z  �      CHARACTER,  getCol  �R      �R      S  [  �      DECIMAL,    getDefaultLayout    �R      S      HS  \  �      CHARACTER,  getDisableOnInit    (S      TS      �S  ]  �      LOGICAL,    getEnabledObjFlds   hS      �S      �S  ^  �      CHARACTER,  getEnabledObjHdls   �S      �S      T  _  �      CHARACTER,  getHeight   �S      T      @T  ` 	 �      DECIMAL,    getHideOnInit    T      LT      |T  a  �      LOGICAL,    getLayoutOptions    \T      �T      �T  b        CHARACTER,  getLayoutVariable   �T      �T      �T  c        CHARACTER,  getObjectEnabled    �T      U      <U  d  /      LOGICAL,    getObjectLayout U      HU      xU  e  @      CHARACTER,  getRow  XU      �U      �U  f  P      DECIMAL,    getWidth    �U      �U      �U  g  W      DECIMAL,    getResizeHorizontal �U      �U      $V  h  `      LOGICAL,    getResizeVertical   V      0V      dV  i  t      LOGICAL,    setAllFieldHandles  DV      pV      �V  j  �      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �V      �V      �V  k  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �V      W      LW  l  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    ,W      pW      �W  m  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �W      �W      �W  n  �      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �W      X      HX  o  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout (X      lX      �X  p  �      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal |X      �X      �X  q  �      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �X       Y      TY  r        LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated 4Y      |Y      �Y  s  !      LOGICAL,    getObjectSecured    �Y      �Y      �Y  t  5      LOGICAL,    createUiEvents  �Y      �Y      ,Z  u  F      LOGICAL,    bindServer                              �Z  �Z      ��                  V  W  �Z              �]u                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �[  �[      ��                  Y  Z  �[              Dt                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �\  �\      ��                  \  ]  �\               t                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �]  �]      ��                  _  `  �]              �t                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �^  �^      ��                  b  c   _              [u                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �_  �_      ��                  e  f  `              �[u                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �`  �`      ��                  h  j  a              �dv                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 $a  
         ��                            ����                            startServerObject                               $b  b      ��                  l  m  <b              4�t                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                (c  c      ��                  o  q  @c              ��t                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  Xc           ��                            ����                            getAppService   Z      �c      �c  v  U      CHARACTER,  getASBound  �c      �c      (d  w 
 c      LOGICAL,    getAsDivision   d      4d      dd  x  n      CHARACTER,  getASHandle Dd      pd      �d  y  |      HANDLE, getASHasStarted |d      �d      �d  z  �      LOGICAL,    getASInfo   �d      �d      e  { 	 �      CHARACTER,  getASInitializeOnRun    �d      e      Pe  |  �      LOGICAL,    getASUsePrompt  0e      \e      �e  }  �      LOGICAL,    getServerFileName   le      �e      �e  ~  �      CHARACTER,  getServerOperatingMode  �e      �e      f    �      CHARACTER,  runServerProcedure  �e      f      Pf  �  �      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   0f      �f      �f  �        LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �f      �f      g  �        LOGICAL,INPUT pcDivision CHARACTER  setASHandle �f      @g      lg  �        LOGICAL,INPUT phASHandle HANDLE setASInfo   Lg      �g      �g  � 	 *      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �g      �g      h  �  4      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �g      4h      dh  �  I      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Dh      �h      �h  �  X      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �h      �h      i  �  j      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �i  �i      ��                  4  8  �i              d�r                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  4j              j  
             ��   \j             (j               �� 
                 Pj  
         ��                            ����                            addMessage                              Hk  0k      ��                  :  >  `k              (�r                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �k             xk               ��   �k             �k               ��                  �k           ��                            ����                            adjustTabOrder                              �l  �l      ��                  @  D  �l              h�u                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  (m             �l  
             �� 
  Pm             m  
             ��                  Dm           ��                            ����                            applyEntry                              <n  $n      ��                  F  H  Tn              `�u                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  ln           ��                            ����                            changeCursor                                ho  Po      ��                  J  L  �o              ��t                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �o           ��                            ����                            createControls                              �p  |p      ��                  N  O  �p              �s                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �q  �q      ��                  Q  R  �q              < u                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �r  �r      ��                  T  U  �r              Du                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �s  �s      ��                  W  X  �s              x�s                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �t  �t      ��                  Z  [  �t              0�s                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �u  �u      ��                  ]  ^  �u              ��t                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �v  �v      ��                  `  a  �v              <�t                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �w  �w      ��                  c  h  �w              �Ns                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  x             �w  
             ��   Dx             x               ��   lx             8x               ��                  `x           ��                            ����                            modifyUserLinks                             \y  Dy      ��                  j  n  ty              (�r                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �y             �y               ��   �y             �y               �� 
                 �y  
         ��                            ����                            removeAllLinks                              �z  �z      ��                  p  q  �z              Pgs                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �{  �{      ��                  s  w  �{              �gs                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <|             |  
             ��   d|             0|               �� 
                 X|  
         ��                            ����                            repositionObject                                X}  @}      ��                  y  |  p}              �r                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �}             �}               ��                  �}           ��                            ����                            returnFocus                             �~  �~      ��                  ~  �  �~              HSs                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �~  
         ��                            ����                            showMessageProcedure                                �  �      ��                  �  �  �              �it                    O   ����    e�          O   ����    R�          O   ����    ��            ��   @�             �               ��                  4�           ��                            ����                            toggleData                              ,�  �      ��                  �  �  D�              �u                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  \�           ��                            ����                            viewObject                              T�  <�      ��                  �  �  l�              �s                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �h      Ă      ��  � 
 �      LOGICAL,    assignLinkProperty  Ђ      ��      0�  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  �  �      CHARACTER,  getChildDataKey ��      ă      �  �  �      CHARACTER,  getContainerHandle  ԃ       �      4�  �        HANDLE, getContainerHidden  �      <�      p�  �        LOGICAL,    getContainerSource  P�      |�      ��  �  1      HANDLE, getContainerSourceEvents    ��      ��      �  �  D      CHARACTER,  getContainerType    Ԅ       �      4�  �  ]      CHARACTER,  getDataLinksEnabled �      @�      t�  �  n      LOGICAL,    getDataSource   T�      ��      ��  �  �      HANDLE, getDataSourceEvents ��      ��      �  �  �      CHARACTER,  getDataSourceNames  ̅      ��      ,�  �  �      CHARACTER,  getDataTarget   �      8�      h�  �  �      CHARACTER,  getDataTargetEvents H�      t�      ��  �  �      CHARACTER,  getDBAware  ��      ��      ��  � 
 �      LOGICAL,    getDesignDataObject ��      �       �  �  �      CHARACTER,  getDynamicObject     �      ,�      `�  �  �      LOGICAL,    getInstanceProperties   @�      l�      ��  �  	      CHARACTER,  getLogicalObjectName    ��      ��      �  �        CHARACTER,  getLogicalVersion   ȇ      �      (�  �  4      CHARACTER,  getObjectHidden �      4�      d�  �  F      LOGICAL,    getObjectInitialized    D�      p�      ��  �  V      LOGICAL,    getObjectName   ��      ��      �  �  k      CHARACTER,  getObjectPage   Ĉ      ��       �  �  y      INTEGER,    getObjectParent  �      ,�      \�  �  �      HANDLE, getObjectVersion    <�      d�      ��  �  �      CHARACTER,  getObjectVersionNumber  x�      ��      ܉  �  �      CHARACTER,  getParentDataKey    ��      �      �  �  �      CHARACTER,  getPassThroughLinks ��      (�      \�  �  �      CHARACTER,  getPhysicalObjectName   <�      h�      ��  �  �      CHARACTER,  getPhysicalVersion  ��      ��      ��  �  �      CHARACTER,  getPropertyDialog   ��      �       �  �        CHARACTER,  getQueryObject   �      ,�      \�  �        LOGICAL,    getRunAttribute <�      h�      ��  �  .      CHARACTER,  getSupportedLinks   x�      ��      ؋  �  >      CHARACTER,  getTranslatableProperties   ��      �       �  �  P      CHARACTER,  getUIBMode   �      ,�      X�  � 
 j      CHARACTER,  getUserProperty 8�      d�      ��  �  u      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    t�      ��      �  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles Ԍ      �      H�  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    (�      l�      ��  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry |�      ؍      �  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      p�      ��  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      Ď      �  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  Ԏ      �      L�  �  �      CHARACTER,  setChildDataKey ,�      X�      ��  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  h�      ��      �  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ď      �      8�  �        LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �      X�      ��  �        LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled t�      ��      �  �  8      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ̐      �      D�  �  L      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents $�      d�      ��  �  Z      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  x�      ��      ��  �  n      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ԑ      �      L�  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents ,�      p�      ��  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Ȓ      ��  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject Ԓ      �      H�  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    (�      p�      ��  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      ��      ��  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ؓ      �      T�  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   4�      p�      ��  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Ȕ      ��  �        LOGICAL,INPUT pcName CHARACTER  setObjectParent ؔ      �      H�  �        LOGICAL,INPUT phParent HANDLE   setObjectVersion    (�      h�      ��  �  .      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    |�      ĕ      ��  �  ?      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ؕ       �      T�  �  P      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   4�      t�      ��  �  d      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ̖       �  �  z      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute ��      $�      T�  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   4�      |�      ��  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ԗ      �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  �      4�      `�  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty @�      ��      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      �      �  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   ��      @�      l�  � 	 �      CHARACTER,INPUT pcName CHARACTER    d�    �	  ��  (�      \      4   ����\                8�                      ��                  �	  �	                  �Tu                       �	  ��        �	  T�  К      l      4   ����l                ��                      ��                  �	  �	                  ��u                       �	  d�  ��    �	  ��  x�      �      4   �����                ��                      ��                  �	  �	                  @�u                       �	  �         �	                                  T     
 
 	       	           � ߱        �  $  �	  ��  ���                           $  �	  8�  ���                       �      
 
       
           � ߱        p�    �	  ��  ��      �      4   �����                �                      ��                  �	  �
                  ��u                       �	  ��  @�  o   �	  
    ,                                 ��  $   �	  l�  ���                       $  @                       � ߱        ��  �   �	  D      ��  �   �	  �      ԝ  �   �	  ,      �  �   �	  �      ��  �   �	        �  �   �	  �      $�  �   �	        8�  �   �	  @      L�  �   �	  �      `�  �   �	  (      t�  �   �	  �      ��  �   �	         ��  �   �	  �      ��  �   �	  �      Ğ  �   �	  T      ؞  �   �	  �      �  �   �	         �  �   �	  x      �  �   �	  �      (�  �   
  (      <�  �   
  �      P�  �   
        d�  �   

  �      x�  �   
        ��  �   
  �      ��  �   
  �      ��  �   
  l      ȟ  �   
  �      ܟ  �   
        �  �   
  X      �  �   
  �      �  �   
        ,�  �   
  D      @�  �   
  �      T�  �   
  �      h�  �   
  8      |�  �   
  t      ��  �   
  �      ��  �   
  �      ��  �    
  (      ̠  �   "
  d      �  �   #
  �      ��  �   $
  �      �  �   %
             �   &
  T                       4�          ��  ��      ��                  �
  �
  ��              l�s                    O   ����    e�          O   ����    R�          O   ����    ��      �      
 
               @!      
               P"                          � ߱        `�  $ �
  С  ���                           O   �
  ��  ��  �"                ̢          ��  Ģ    ��                                              ��                            ����                            #  �L      �      x�      @     Ԣ                      W Т  g                     0�      ��  �      �"      4   �����"                �                      ��                    �                  ��t                         ��  ,�  �     �"      @�  �     p#      T�  �     �#      h�  �     h$      |�  �     �$      ��  �     `%      ��  �     �%      ��  �     P&      ̤  �     �&      �  �     H'      ��  �     �'      �  �     8(      �  �     �(          �     0)      �    �  L�  ȥ      �)      4   �����)                إ                      ��                  �  /                  ��u                       �  \�  �  �   �   *       �  �   �  t*      �  �   �  �*      (�  �   �  d+      <�  �   �  �+      P�  �   �  L,      d�  �   �  �,      x�  �   �  <-      ��  �   �  �-      ��  �   �  $.      ��  �   �  �.      Ȧ  �   �  /      ܦ  �   �  �/      �  �   �  0      �  �   �  �0      �  �   �  �0      ,�  �   �  x1      @�  �   �  �1      T�  �   �  p2      h�  �   �  �2      |�  �   �  h3      ��  �   �  �3      ��  �   �  `4      ��  �   �  �4      ̧  �   �  X5      �  �   �  �5      ��  �   �  P6          �   �  �6      $�    ;  $�  ��      47      4   ����47                ��                      ��                  <  �                  �u                       <  4�  Ĩ  �   ?  �7      ب  �   @  8      �  �   A  �8       �  �   B   9      �  �   D  t9      (�  �   E  �9      <�  �   G  \:      P�  �   H  �:      d�  �   I  ;      x�  �   J  H;      ��  �   K  �;      ��  �   L  �;      ��  �   M  l<      ȩ  �   N  �<      ܩ  �   P  \=      �  �   Q  �=      �  �   R  D>      �  �   S  �>      ,�  �   T  <?      @�  �   U  x?      T�  �   W  �?      h�  �   X  `@      |�  �   Y  �@      ��  �   Z  A      ��  �   [  LA      ��  �   \  �A      ̪  �   ]  B      �  �   ^  @B      ��  �   _  |B      �  �   `  �B      �  �   a  �B      0�  �   b  0C      D�  �   c  lC      X�  �   e  �C      l�  �   f  D      ��  �   g  XD      ��  �   h  �D      ��  �   i  �D      ��  �   j  E      Ы  �   k  HE      �  �   l  �E      ��  �   m  �E      �  �   n  lF       �  �   o  �F      4�  �   p  TG      H�  �   q  �G      \�  �   r  LH      p�  �   s  �H      ��  �   t  DI      ��  �   u  �I      ��  �   v  <J      ��  �   w  xJ      Ԭ  �   x  �J      �  �   y  0K      ��  �   z  lK      �  �   {  �K          �   |  L      |�  $  �  P�  ���                       �L     
 
                   � ߱        �    2  ��  ��      �L      4   �����L      /   3  ԭ     �                          3   �����L            �                      3   �����L  h�    <  0�  ��  ��  �L      4   �����L  	              ��                      ��             	     =  �                  ��t                       =  @�  Ю  �   A  <M      (�  $  B  ��  ���                       hM     
 
 	       	           � ߱        <�  �   C  �M      ��  $   E  h�  ���                       �M  @         �M              � ߱        P�  $  H  ��  ���                       N      
                   � ߱        xN     
 
               �N      
               DP  @        
 P              � ߱        �  V   R  �  ���                        PP      
               �P      
               �P      
                   � ߱        p�  $  n  |�  ���                       �Q     
 
               �Q      
               LS  @        
 S              � ߱         �  V   �  �  ���                        XS     
 
               �S      
               $U  @        
 �T              � ߱            V   �  ��  ���                        
              `�                      ��             
     �  `                  	s                       �  ,�  0U     
 
               �U      
               �V  @        
 �V          `W  @        
  W          �W  @        
 �W           X  @        
 �W              � ߱            V   �  ��  ���                        adm-clone-props �  ��              �    ! A     `                          \  :$                     start-super-proc    ��  ��  �           �    " B                                  [$                      �    x  ��  ��      �[      4   �����[      /   y  ��     д                          3   �����[            �                      3   �����[  X�  $  �  ,�  ���                       �[      
                   � ߱        �    �  t�  �  ��  \      4   ����\                d�                      ��                  �  �                  L,a                       �  ��  ,\      
               @\      
               T\      
                   � ߱            $  �   �  ���                             �  ��  �      l\      4   ����l\  �\      
                   � ߱            $  �  ��  ���                       �    �  0�  @�  ��  �\      4   �����\      $  �  l�  ���                       �\      
                   � ߱            �   �  �\      ]     
 
               �]      
               �^  @        
 �^              � ߱        <�  V   �  ��  ���                        P�  �     �^      �    �  l�  |�      ,_      4   ����,_      /   �  ��     ��                          3   ����<_            ظ                      3   ����\_  ��  $  �  �  ���                       x_      
                   � ߱        �_     
 
                `      
               pa  @        
 0a              � ߱        й  V   �  @�  ���                        ��    "  �  h�      |a      4   ����|a                x�                      ��                  #  &                  ��r                       #  ��      g   $  ��         g�T�                           X�          (�  �      ��                  %      @�              d�r                    O   ����    e�          O   ����    R�          O   ����    ��          /  %  ��     ��  �a                      3   �����a  Ļ     
   ��                      3   �����a         
   �                      3   �����a    ��                              ��        �                  ����                                        ��              C      ��                      g                               ��  g   (  ȼ          g�	\�                           ��          `�  H�      ��                  (  *  x�              ��b                    O   ����    e�          O   ����    R�          O   ����    ��          /  )  ��     ̽  �a                      3   �����a            �                      3   �����a    ��                              ��        �                  ����                                        ܼ              D      ��                      g                               ��  g   ,  о          g�	d�                           ��          h�  P�      ��                  ,  .  ��              ��b                    O   ����    e�          O   ����    R�          O   ����    ��          /  -  Ŀ     Կ  b                      3   ���� b            ��                      3   ����$b    ��                              ��        �                  ����                                        �              E      �                      g                                �    E  ��  X�      @b      4   ����@b                h�                      ��                  F  e                  �d                       F  ��  ��  /   G  ��     ��                          3   ����Pb            ��                      3   ����pb  ��  /  I   �     �  �b                      3   �����b  @�     
   0�                      3   �����b  p�        `�                      3   �����b  ��        ��                      3   �����b            ��                      3   �����b  ��    Q  ��  ��      c      4   ����c      /  W  (�     8�  �c                      3   �����c  h�     
   X�                      3   �����c  ��        ��                      3   �����c  ��        ��                      3   �����c            ��                      3   �����c        ]  �  $�      d      4   ����d      /  `  P�     `�  \d                      3   ����<d  ��     
   ��                      3   ����dd  ��        ��                      3   ����ld  ��        ��                      3   �����d            �                      3   �����d  ��    i  <�  ��      �d      4   �����d                ��                      ��                  j  m                  ,d                       j  L�      g   k  ��         g���        �d                  ��          x�  `�      ��                  l      ��              ̆�                    O   ����    e�          O   ����    R�          O   ����    ��          /  l  ��     ��  �d                      3   �����d  �     
   �                      3   ���� e         
   4�                      3   ����e    ��                            ����                                        ��              F      D�                      g                               x�     q  e                                     $e     
 
               �e      
               �f  @        
 �f              � ߱        �  V   �  �  ���                        g     
 
               �g      
               �h  @        
 �h              � ߱        4�  V     ��  ���                        ��    <  P�  `�      �h      4   �����h      $   =  ��  ���                       Di  @         0i              � ߱        ��  g   i  ��         g�0�        Xi  g�0�        di                  ��          |�  d�      ��                  j  o  ��              Tǔ                    O   ����    e�          O   ����    R�          O   ����    ��            n  ��  ��      pi      4   ����pi      O  n  ������  �i    ��                            ����                                        ��              G      ��                      g                               8�  g   v  ��         g6��         �i                  l�          <�  $�      ��                  w  |  T�              �ɔ                    O   ����    e�          O   ����    R�          O   ����    ��      ��    z  �i  }          O  {  ������  �i    ��                            ����                                        ��              H      ��                      g                               ��  g   �  P�         g}x�                                       ��  ��      ��                  �  �   �              �ʔ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                              ��        �                  ����                                        d�              I      �                      g                               X�  g   �  ��         g"��                           ��          ��  l�      ��                  �  �  ��              hja                    O   ����    e�          O   ����    R�          O   ����    ��             
                   � ߱        �  $   �  ��   �                           s   �  8�        P�      p�          ��  d�  ��       ��                            7   ����           ��                <j   �            �                  6   �         (�   ��               <j   �            �                                                                ��  |�           �i  j  j  ,j           j  j  $j  4j                      D�   `�          �j  �j  �j  �j  �j              ��  <�       ��                            7   ����          ��               Tk   �            ��                  6   �        ��   ��         ��  Tk   �            ��                                                         k   k  	 k                  �  �           $k  4k  Dk           ,k  <k  Lk         �   
        
 ��   ��          �k  �k  �k                 �i   �i   �i   �i    ��  4�    ��                              ��        �                  ����                            \                         ��                 �              J      x�             ��      g                               ��  g   �  p�         g"D�                            d�          �  ��      ��                  �  �   �              8�d                    O   ����    e�          O   ����    R�          O   ����    ��             
                   � ߱        ��  $   �  8�   �                       ��  /   �  ��                                 3   �����k  �    �  ��  ��      �k      4   �����k      O  �  ������  l  ��  $  �  <�  ���                        l      
                   � ߱        ,l  �              � ߱        ��  Z   �  h�   �                        ��    �  8l             �  Dl         ��                              ��        �                  ����                                        ��              K      ��                      g                               T�  g   �  ��         g ��                           ��          P�  8�      ��                  �  �  h�              �d                    O   ����    e�          O   ����    R�          O   ����    ��            �  Xl  }        ��                              ��        �                  ����                                        ��              L      ��                      g                               ��  g   �  l�         g4��                           4�          �  ��      ����                �  �  �              �a                    O   ����    e�          O   ����    R�          O   ����    ��      �  A  �       # ��   ��         ��  �l                                        ll   xl   �l   �l                   ��  ��           �l  �l  �l           �l  �l  �l         �            ��   ��    �    �  ,�  <�  ��  Hm      4   ����Hm      $  �  h�  ���                       Pm      
                   � ߱            $  �  ��  ���                       �m      
                   � ߱        �m  �              � ߱            Z   �  ��   �                          ��                              ��        �                  ����                                #              ��              M      8�                      g                               D�  g   �  �         g ��                           ��          ��  ��      ����                �  �  ��              Dwa                    O   ����    e�          O   ����    R�          O   ����    ��      4�  $   �  �  ���                       �m  @         �m              � ߱        ��  A  �       $ ��   ��         ��  n                                        �m   �m                   ��  ��           �m   n           �m  n         �            ��   ��          �  �  (�      @n      4   ����@n      $   �  T�  ���                       \n  @         Hn              � ߱          ��                              ��        �                  ����                                $              (�              N      ��                      g                               `�  g   �  \�         g��                           ��          ��  ��      ��                  �  �  �              (�c                    O   ����    e�          O   ����    R�          O   ����    ��      hn                     tn                     �n                         � ߱        ��  $  �  $�  ���                        �  /   �  ��     ��                          3   �����n            �                      3   �����n        �  <�  L�      �n      4   �����n      $   �  x�  ���                       �n  @         �n              � ߱          ��                              ��        �                  ����                                        p�              O      ��                      g                                       |�  ��       o      4   ���� o                l�                      ��                    0                  0Na                         ��  o  @                     <o  @         (o          do  @         Po              � ߱        ��  $     �  ���                       ��  g     ��         gn8�      }                      x�          H�  0�      ��                      `�              �Na                    O   ����    e�          O   ����    R�          O   ����    ��      ��  /    ��                                 3   ����po          ��  ��      �o      4   �����o      O    ������  �o    ��                            ����                                        ��              P      ��                      g                               h�  g     ��         g!�         �o                  ��          D�  ,�      ��                      \�              Oa                    O   ����    e�          O   ����    R�          O   ����    ��      �o  @                         � ߱            $    t�  ���                         ��                            ����                                        ��              Q      ��                      g                               ��  /     ��                                 3   �����o        !  ��  <�      p      4   ����p                ��                      ��                  !  .                  ��b                       !  ��                ��          ��  ��      ��                 %  ,                  �b                       %  L�      O   %    ��          O   %    ��      4�  /   )  $�                                 3   ����p        *  P�  `�      <p      4   ����<p      k   +  |�              }       n        �   adm-create-objects  �  ��                      R      �                               �&                     disable_UI  ��  �                      S      <                              �&  
                   enable_UI   �  l�                      T      l             �              �&  	                   exitObject  x�  ��                      U      �                               �&  
                   Genera-NC   ��  <�          �   ,!  & ' V     �!                          �!  �(  	                   initializeObject    H�  ��              l    , W     �                          �  �(                     proc_AplicaDoc-2    ��  �  �       �      -   X     (                          $  3)                                     `�          �  ��      ��                  @  K   �               �c                    O   ����    e�          O   ����    R�          O   ����    ��      D)   0                   8�          ��    F  |�  ��      Ԃ      4   ����Ԃ      O   F  ��  ��  ��  ��    G  ��  ��      �      4   �����      O   G  ��  ��  0�      O   I  ��  ��  <�             0  T�          D�  L�    4�                                    �  0     ��                            ����                            (�  p
  D�  ��       �    0 Y     \�                       X�  Z)                     �   �A/CN/C �����   �  ���      �   �    ���  �         X�  8   ����   h�  8   ����   x�  8   ����/   ��  8   ����/   ��  8   ����   ��  8   ����   ��  8   ����+   ��  8   ����+    �  + 	 ��  8   ����*   ��  8   ����*   �  8   ����)   �  8   ����)   H�  )  (�  8   ����   8�  8   ����   P�  8   ����(   `�  8   ����(   p�  (  x�  8   ����%   ��  8   ����%   ��  %  ��  8   ����$   ��  8   ����$   ��  $  ��  8   ����#   ��  8   ����#   ��  #  ��  8   ����    �  8   ����         �  8   ����    �  8   ����       8   ����       8   ����       @�  L�      toggleData  ,INPUT plEnabled LOGICAL    0�  x�  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  h�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  �  �      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  X�  d�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE H�  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  D�  X�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    4�  ��  ��      hideObject  ,   ��  ��  �      editInstanceProperties  ,   ��  �  ,�      displayLinks    ,   �  @�  P�      createControls  ,   0�  d�  t�      changeCursor    ,INPUT pcCursor CHARACTER   T�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  @�  L�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER 0�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��   �  �      unbindServer    ,INPUT pcMode CHARACTER ��  8�  L�      startServerObject   ,   (�  `�  p�      runServerObject ,INPUT phAppService HANDLE  P�  ��  ��      restartServerObject ,   ��  ��  ��      initializeServerObject  ,   ��  ��  �      disconnectObject    ,   ��  �  ,�      destroyServerObject ,   �  @�  L�      bindServer  ,   0�  `�  p�      processAction   ,INPUT pcAction CHARACTER   P�  ��  ��      enableObject    ,   ��  ��  ��      disableObject   ,   ��  ��  ��      applyLayout ,   ��  �  �      viewPage    ,INPUT piPageNum INTEGER    ��  <�  H�      viewObject  ,   ,�  \�  d�      toolbar ,INPUT pcValue CHARACTER    L�  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  ��  ��      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  �  $�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  �  l�  x�      notifyPage  ,INPUT pcProc CHARACTER \�  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  ��  ��      initializeVisualContainer   ,   ��  �  �      hidePage    ,INPUT piPageNum INTEGER    ��  @�  P�      destroyObject   ,   0�  d�  p�      deletePage  ,INPUT piPageNum INTEGER    T�  ��  ��      createObjects   ,   ��  ��  ��      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  D�  P�      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  4�  ��  ��      changePage  ,   p�  ��  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
   %              %       	       "      "      "      "  )    �            $     "              "      "      %              %              %              %                  �     }        �G� �   �G%              �    1   %        %        %        %       %        %       %               %               %               %              %              %              %               %              
�    � %              %              %              %         %          � �      
�             �G%              %               %     _corre-program  %      ENTRY   
"   
   %      ENTRY   
"   
   
"   
 ��       �    �A� �   �
"   
 ��        �     %               
"   
 ��        $     %               (    S    �     }         � �    %               %                   �     }         � �    %     bin/_inslook.r  �     }        �"      � �         �     }         � �    
"   
 �    �        H     %              � 	     
"   
   (    S    �     }         � �    %               %                   �     }         � �    
�     }        �G
�     }        � %     _busca-lookup   �     }        �"      "          "    �� �    �
"   
 ��        �     %               
"   
 ��        �     %               
"   
 =�            6@� 		     � 	     � 	   �� +	     � 0	   �%               
"   
 �    �        �     � A	    
"   
 ��        �     %              
"   
 ��        �     �     }         
"   
 ��                   �     }         �     }        �
"   
 ��        p    ��     }        �
"   
 ��        �     %               
"   
   �        �     %              , (   (     
�     }        �
"   
 �    �     }        �G� H	   �G
"   
 ��        h	     %               
"   
 ��        �	     %               %      notify  � Q	     %      notify  � b	     "    �"    �&    &    &    &        %              %              *    "      "      � �	   �"    �&    &    &    &        %              %              *    "      "      � �    �� �      �    }        �� �	     "      � 	     %     bin/_calc.r     �  %              
"   
   �        �    B�  � 0	     %     bin/_calenda.r      �  %              
"   
   �            B�  � �	     %     recoge-parametros �"      "          "    �%              
"   
   �        �    B"      %     procesa-parametros �    }        �� �          
" 
  
 �
�    
" 
  
 �
" 
  
 =    �        ,     �        8    
" 
  
   �        t         �     }        �%              
" 
  
 �
" 
  
 =    �        �     �        �    
" 
  
   �                 �     }        �%              � 
"    
 �%              � �  �         �      T     @     $              
�    � �   �     
" 
  
 r� �   �     
�             �G                      
�            � �   r
"    
 �
�H T   %              �     }        �GG %              � 
" 
 	 
   P �L 
�H T   %              �     }        �GG %              
" 
  
   �            7%               
" 
  
 ��           8    1�   
 ��    �%               o%   o           �     �
" 
  
 ��           �    1�    ��    �%               o%   o           � +   �
" 
  
 ��                1� 2  
 ��    �%               o%   o           � =   �
" 
  
 ��           �    1� I   ��    �%               o%   o           � W   �
" 
  
 ��               1� ^   ��    �%               o%   o           � m   �
" 
  
 ��           |    1� �   �� �   �%               o%   o           %               
" 
  
 ��          �    1� �   �� �     
" 
  
 ��           4    1� �   ��    �%               o%   o           � �  e �
" 
  
 ��           �    1� (   ��    �%               o%   o           � 7  [ �
" 
  
 ��               1� �   �� �   �%               o%   o           %               
" 
  
 ��           �    1� �   �� �   �%               o%   o           %               
" 
  
 ��               1� �   �� �   �%               o%   o           %              
" 
  
 ��          �    1� �   �� �     
" 
  
 ��           �    1� �  
 �� �   �%               o%   o           %               
" 
  
 ��           H    1� �   ��    �%               o%   o           �     �
" 
  
 ��          �    1� �   �� �     
" 
  
 ��           �    1� �   ��    �%               o%   o           � 
  t �
" 
  
 ��          l    1�   
 �� �     
" 
  
 ��           �    1� �   ��    �%               o%   o           � �  � �
" 
  
 ��               1� (   ��    �%               o%   o           �     �
" 
  
 ��           �    1� ?  
 �� J   �%               o%   o           %               
" 
  
 u�               1� N   u� �   �%               o%   o           %               
" 
  
 r�           �    1� V   r�    �%               o%   o           �     u
" 
  
 r�           �    1� g   r�    �%               o%   o           o%   o           
" 
  
 u�           x    1� w  
 u�    �%               o%   o           �     v
" 
  
 r�           �    1� �   r� �  	 �%               o%   o           � �  / u
" 
  
 ��          `    1� �   �� �  	   
" 
  
 v�           �    1� �   v� �  	 �o%   o           o%   o           �     v
" 
  
 ��              1� �   �� �  	   
" 
  
 ��           L    1�    �� �  	 �o%   o           o%   o           �     �
" 
  
 ��          �    1�    �� �     
" 
  
 ��          �    1�    �� �  	   
" 
  
 ��          8    1� ,   �� �  	   
" 
  
 ��          t    1� 9   �� �  	   
" 
  
 s�           �    1� G   s� �   �o%   o           o%   o           %              
" 
  
 ��          ,    1� X   �� �  	   
" 
  
 ��          h    1� f  
 �� q     
" 
  
 ��          �    1� y   �� �  	   
" 
  
 ��          �    1� �   �� �  	   
" 
  
 ��              1� �   �� �  	   
" 
  
 ��          X    1� �   �� �  	   
" 
  
 ��          �    1� �  	 �� �  	   
" 
  
 ��          �    1� �   �� �  	   
" 
  
 ��               1� �   �� �  	   
" 
  
 r�           H     1� �   r�    �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
" 
  
   
" 
  
 t
" 
  
   
" 
  
 =(�  L ( l       �        !    �� �   � P   �        !    �@    
� @  , 
�       (!    ��      p�               �L
�    %              � 8      4!    � $         �           
�    � )     
" 
  
 �� @  , 
�       D"    �� 2  
 �p�               �L"       P �L 
�H T   %              �     }        �GG %              
" 
  
 v�           �"    1� ,  
 v�    �%               o%   o           �     v
" 
  
 v�           d#    1� 7  
 v�    �%               o%   o           o%   o           
" 
  
 r�           �#    1� B   r� �   �%               o%   o           o%   o           
" 
  
 r�           \$    1� K   r� �   �%               o%   o           %               
" 
  
 u�           �$    1� Z   u� �   �%               o%   o           %               
" 
  
 ��           T%    1� g   ��    �%               o%   o           �     u
" 
  
 s�           �%    1� n   s� �   �%               o%   o           %              
" 
  
 s�           D&    1� �   s� �   �%               o%   o           o%   o           
" 
  
 u�           �&    1� �   u�    �%               o%   o           o%   o           
" 
  
 r�           <'    1� �  	 r�    �%               o%   o           �     v
" 
  
 r�           �'    1� �   r�    �%               o%   o           o%   o           
" 
  
 s�           ,(    1� �   s�    �%               o%   o           o%   o           
" 
  
 u�           �(    1� �   u� �   �%               o%   o           %               
" 
  
 u�           $)    1� �   u� �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
" 
  
 t�           �)    1� �   t� �  	 �%               o%   o           �     t
" 
  
 u�           h*    1� �   u� �  	 �%               o%   o           �     t
" 
  
 v�           �*    1� �   v� �   �%               o%   o           %               
" 
  
 ��           X+    1�    �� �  	 �%               o%   o           �     v
" 
  
 s�           �+    1�    s� �  	 �%               o%   o           �     �
" 
  
 s�           @,    1� )   s� �   �%               o%   o           %               
" 
  
 r�           �,    1� 7   r� �  	 �%               o%   o           �     s
" 
  
 s�           0-    1� F   s� �  	 �%               o%   o           �     r
" 
  
 t�           �-    1� U   t� �  	 �%               o%   o           �     s
" 
  
 t�           .    1� c   t� �  	 �%               o%   o           o%   o           
" 
  
 v�           �.    1� q   v� �  	 �%               o%   o           �     u
" 
  
 ��           /    1� �   �� �  	 �%               o%   o           �     v
" 
  
 s�           |/    1� �  	 s� q   �%               o%   o           %               
" 
  
 s�           �/    1� �   s� q   �%               o%   o           %               
" 
  
 s�           t0    1� �   s� �   �%               o%   o           o%   o           
" 
  
 r�           �0    1� �   r� �   �%               o%   o           o%   o           
" 
  
 t�           l1    1� �   t� �   �%               o%   o           %               
" 
  
 u�           �1    1� �   u� �   �%               o%   o           %               
" 
  
 v�           d2    1� �   v� �   �%               o%   o           %               
" 
  
 ��           �2    1� �   ��    �%               o%   o           %       
       
" 
  
 ��           \3    1� 
   ��    �%               o%   o           o%   o           
" 
  
 u�           �3    1�    u�    �%               o%   o           %              
" 
  
 u�           T4    1� "   u�    �%               o%   o           o%   o           
" 
  
 u�           �4    1� .   u�    �%               o%   o           %              
" 
  
 v�           L5    1� ;   v�    �%               o%   o           o%   o           
" 
  
 u�           �5    1� H   u�    �%               o%   o           %              
" 
  
 u�           D6    1� P   u�    �%               o%   o           o%   o           
" 
  
 ��           �6    1� X   �� �  	 �%               o%   o           �     sP �L 
�H T   %              �     }        �GG %              
" 
  
 s�           �7    1� j   s� J   �%               o%   o           %               
" 
  
 s�           8    1� v   s� J   �%               o%   o           o%   o           
" 
  
 s�           �8    1� �   s�    �%               o%   o           �     u
" 
  
 u�           �8    1� �   u�    �%               o%   o           � �  - s
" 
  
 v�           h9    1� �   v�    �%               o%   o           �     v
" 
  
 u�           �9    1� �   u�    �%               o%   o           � 
    v
" 
  
 ��          P:    1� (    �� �     
" 
  
 r�           �:    1� 9    r�    �%               o%   o           �     t
" 
  
 ��           ;    1� E   
 �� �     
" 
  
 ��          <;    1� P    �� �     
" 
  
 s�           x;    1� ]    s� �  	 �%               o%   o           �     u
" 
  
 u�           �;    1� j    u�    �%               o%   o           �     s
" 
  
 v�           `<    1� w    v� �   �%               o%   o           o%   o           
" 
  
 u�           �<    1� �    u�    �%               o%   o           � �   ! r
" 
  
 s�           P=    1� �    s�    �%               o%   o           �     u
" 
  
 ��           �=    1� �    ��    �%               o%   o           � �    s
" 
  
 ��           8>    1� �   	 �� J   �%               o%   o           o%   o           
" 
  
 u�           �>    1� �    u� �   �%               o%   o           %               
" 
  
 ��          0?    1� �    �� �     
" 
  
 u�           l?    1� !   u�    �%               o%   o           �  !   v
" 
  
 r�           �?    1� /!   r� �  	 �%               o%   o           �     v
" 
  
 u�           T@    1� <!   u� �  	 �%               o%   o           �     r
" 
  
 ��          �@    1� L!   �� �     
" 
  
 ��          A    1� ^!   �� �  	   
" 
  
 ��           @A    1� q!   �� �   �o%   o           o%   o           %               
" 
  
 ��          �A    1� �!   �� �     
" 
  
 ��          �A    1� �!   �� �  	   
" 
  
 ��          4B    1� �!   �� �  	   
" 
  
 ��          pB    1� �!   �� �  	   
" 
  
 ��          �B    1� �!   �� �  	   
" 
  
 ��          �B    1� �!   �� �  	   
" 
  
 ��          $C    1� �!   �� �     
" 
  
 u�           `C    1� "   u�    �%               o%   o           � "  4 s
" 
  
 ��          �C    1� P"   �� �     
" 
  
 ��          D    1� ]"   �� �     
" 
  
 ��          LD    1� m"   �� �     
" 
  
 ��          �D    1� z"   �� �  	   
" 
  
 ��          �D    1� �"   �� �  	   
" 
  
 ��           E    1� �"   �� �  	   
" 
  
 ��          <E    1� �"   �� �     
" 
  
 s�           xE    1� �"   s� �  	 �%               o%   o           �     v
" 
  
 s�           �E    1� �"   s� �  	 �%               o%   o           �     s
" 
  
 s�           `F    1� �"   s� �  	 �%               o%   o           �     s
" 
  
 u�           �F    1� �"   u� �  	 �%               o%   o           �     s
" 
  
 t�           HG    1� #   t� �   �%               o%   o           %               
" 
  
 t�           �G    1� #   t� �   �%               o%   o           o%   o           
" 
  
 r�           @H    1� ##   r� �   �%               o%   o           %               
" 
  
 u�           �H    1� 3#   u� �   �%               o%   o           %               
" 
  
 v�           8I    1� ?#   v� �   �%               o%   o           o%   o           
" 
  
 s�           �I    1� Z#   s� �   �%               o%   o           %               
" 
  
 ��          0J    1� h#   �� �  	   
" 
  
 r�           lJ    1� v#   r� �   �%               o%   o           %              
" 
  
 ��          �J    1� �#   �� �  	   
" 
  
 ��          $K    1� �#   �� �  	   
" 
  
 ��          `K    1� �#  
 �� �  	   
" 
  
 u�           �K    1� �#   u� �  	 �%               o%   o           � #   u
" 
  
 v�           L    1� �#   v� �  	 �%               o%   o           �     v
" 
  
    " 
 
  �%     start-super-proc ��%     adm2/smart.p h=P �L 
�H T   %              �     }        �GG %              
" 
  
   �       0M    6� �     
" 
  
   
�        \M    8
" 
 	 
   �        |M    ��     }        �G 4              
" 
 	 
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout =
�H T   %              �     }        �GG %              
" 
  
 =
" 
  
 �
" 
  
 =
" 
  
   (�  L ( l       �        �N    �� �   � P   �        �N    �@    
� @  , 
�       �N    ��    =p�               �L
�    %              � 8      �N    � $         �           
�    � )   =
" 
  
 �p� @  , 
�       �O    �� �   �p�               �L" 
   , �   � �#   u� �#   ��     }        �A      |    " 
     � �#   v%              (<   \ (    |    �     }        �A�  $   �A" 
   u    " 
   =" 
   u  < " 
   =" 
   u(    |    �     }        �A�  $   �A" 
   u
�H T   %              �     }        �GG %              
" 
  
 =
" 
  
 �
" 
  
 =
" 
  
   (�  L ( l       �        �Q    �� �   � P   �        �Q    �@    
� @  , 
�       �Q    ��    =p�               �L
�    %              � 8      �Q    � $         �           
�    � )   =
" 
  
 �p� @  , 
�        S    ��   
 �p�               �L" 
   , 
�H T   %              �     }        �GG %              
" 
  
 =
" 
  
 �
" 
  
 =
" 
  
   (�  L ( l       �        �S    �� �   � P   �        �S    �@    
� @  , 
�       �S    ��    =p�               �L
�    %              � 8      �S    � $         �           
�    � )   =
" 
  
 �p� @  , 
�       �T    �� �   �p�               �L
" 
  
 , 
�H T   %              �     }        �GG %              
" 
  
   
" 
  
 b
" 
  
   
" 
  
   (�  L ( l       �        |U    �� �   � P   �        �U    �@    
� @  , 
�       �U    ��      p�               �L
�    %              � 8      �U    � $         �           
�    � )     
" 
  
 �p� @  , 
�       �V    �� 2  
 �p�               �L%     SmartWindow 
" 
  
   p� @  , 
�       W    �� I     p�               �L%      WINDOW  
" 
  
  p� @  , 
�       tW    ��     p�               �L%               
" 
  
  p� @  , 
�       �W    �� �    p�               �L(        �       �       �       �     }        �A
�H T   %              �     }        �GG %              
" !  
 v (   � 
" !  
 =    �        �X    �� �   �
" !  
   � 8       Y    � $         �           
�    � )   =
" !  
   �        XY    �
" !  
   �       xY    /
" !  
   
" !  
   �       �Y    6� �     
" !  
   
�        �Y    8
" !  
   �        �Y    �
" !  
   �       Z    �
" !  
   p�    � )$   u
�    �     }        �G 4              
" !  
 ߱G %              G %              
�     }        �
" "  
    (   � 
" "  
 =    �        �Z    �A" "   �A
" "  
   
�         [    �@ � 
" "  
 v" "     �       }        �
" "  
 �%              %                " 
 
  �%     start-super-proc ��%     adm2/appserver.p �u�    � �$     
�    �     }        �%               %      Server  - �     }        �    " 
   s�     �%                   " 
   s�     �%      NONE    p�,  8         $     " 
   r        � �$   =
�    
�H T   %              �     }        �GG %              
" 
  
 =
" 
  
 �
" 
  
 =
" 
  
   (�  L ( l       �        `]    �� �   � P   �        l]    �@    
� @  , 
�       x]    ��    =p�               �L
�    %              � 8      �]    � $         �           
�    � )   =
" 
  
 �p� @  , 
�       �^    �� �   �p�               �L" 
   , p�,  8         $     " 
   r        � �$   =
�     " 
 
  �%     start-super-proc ��%     adm2/visual.p =�   � �     � �$     � �$  D   
�H T   %              �     }        �GG %              
" 
  
 =
" 
  
 �
" 
  
 =
" 
  
   (�  L ( l       �        �_    �� �   � P   �        �_    �@    
� @  , 
�       `    ��    =p�               �L
�    %              � 8      `    � $         �           
�    � )   =
" 
  
 �p� @  , 
�       $a    �� 7   �p�               �L" 
   , � 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP g=%     processAction   
�    %     CTRL-PAGE-DOWN  " 
 
  �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents s%      initializeDataObjects s0 0   A    �    � �%   s
�    � �%   �A    �    � �%     
�    � �%   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents c%     buildDataRequest ent0 A    �    � �%   �
�    � �%   u%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
"    
 �
" 
  
 �%     contextHelp 
"    
   
�    
�    %               
�H T   %              �     }        �GG %              
" 
  
 =
" 
  
 �
" 
  
 =
" 
  
 r(�  L ( l       �        pe    �� �   � P   �        |e    �@    
� @  , 
�       �e    ��    =p�               �L
�    %              � 8      �e    � $         �    =     
�    � )   �
" 
  
 �p� @  , 
�       �f    �� L!   �p�               �L
�             �G
�H T   %              �     }        �GG %              
" 
  
 =
" 
  
 �
" 
  
 =
" 
  
 =(�  L ( l       �        Pg    �� �   � P   �        \g    �@    
� @  , 
�       hg    ��    =p�               �L
�    %              � 8      tg    � $         �    =     
�    � )   =
" 
  
 �p� @  , 
�       �h    �� #   �p�               �L%              (        �     }        �G� �   �G� 
" 
  
 =
" 
  
   �        $i    �%              
" 
  
 �
" 
  
 ��     }        �%               
" 
  
 �%      CLOSE   %               � �%   �"     �"    �" 
   �&    &    &    &    &    &    &    &    d    @            "       &        "       &        "       &        "       &    "       "       "       "       "       "     �"   '  �"   (  �&    &    &    &    & 	   & 	   @            "     &        "      &        "      & 	   "      "      "      % 	    Genera-NC =    �  � �%  	 =%               � �      " 
   ��  &     %      CHOOSE  %      Tab     "     �"    �"     � �     }        B&    &    &    &    &    &    T    0        %              %                  " #     &    %              * #              " #     � =&         " #     � A&     � �      " 
   a�            B� �      "     ��     }        B&    &    &    &        %              %              * $   �            B" $     � �    a� �    =� �    s%     vtagn/c-gn-clie-01 � d&         "    s%              �     }        B"      � 
" 
  
 �
" 
  
 s
" 
  
 =�        o    %%              
�     }        �
" 
  
   %     destroyObject       �     }        �    �  � x&  	   %               
" 
  
 �
�    %     createObjects    �     }        �%     initializeObject � �     }        �(        �     }        �G� �   �G� 
" 
  
 =
" 
  
   �     }        �
�    
" 
  
 =" 
   t" 
   =" 
     " 
     
" 
  
 t� �%   �"     �"    �" 
   �&    &    &    &    &    &    &    &    d    @            "       &        "       &        "       &        "       &    "       "       "       "       "       "     �"   '  �"   (  �&    &    &    &    & 	   & 	   @            "     &        "      &        "      & 	   "      "      "      
" 
  
   %      CLOSE   %               "     �&    &     *    � �%  	   +  &     * (   � \'  #   � �%  	   " (     " (     "       " 
     � �'  "   %      
       %      
       "      "      " &     %      
       " &     %      
       � �'     " & 	        " & 	  v%               � �%  	   � �%  	   � �%  	    *    � �'     � �%  	   " &   v:       " '     %                  " '   v%              "     �"    �" &   �&    &    &    &    &    &    0        %              %              %              (        " '   b%               * #   � �%  	       " #     %               (         " #     %                   " #     " #     � �'  ( �" #   �%      
       � (      "      � 6(     " &     � �%  	       " #     %              (         " #     %                   " #     " #         " #     %               " #     %              "       "       � �     � <(                " #     � =&         " #     � A&    "  '    "  (    "      ((       "      %              " '     " '    +  +  " 	      " &     " &     +  � �%     "     �"  "  �&    &    &    &        %              %              * )   " )          " #     %              %              � Y(     %              "      "      %              � �   �"     �" *   �&    &    &    &    &    &    0        %              %              %              " +   �%               t   " *     ( D       "      %       d         (   %                  "      %       d       %               %               ((        " *     %                  " *     " *     %               ((        " *     %                   " *     " *     %               " * 
    " *         "      %              "          "      "          "      "      "      %     proc_AplicaDoc-2 l�"    �"    �"    �"    �" '   �" '   �    �  � �%  	 =� �%  	   *    *    *    * *   * #   � {(     "     �&    &    � �      %              "       "       "      &    &    &    &    &    &    L    0        %              %              %                  " #     &        " ,   a� �    �    " #     � =&                 " ,     � �(         " #     � =&     �            F" ,     T    %              �            F"     �"    �"     � " 
     &    &    &    &    &    &    T    0        %              %                  " #     &    %              * #              " #     � =&         " #     � A&     %      SUPER   � �%  	   � �%  	   "     �" -   �" -   �&    &    &    &    &    &    0        %              %              %               * .   � )  	   " -     " -     � )     � �%  	   "       "       "      " -     " .     " .     " .     " .     " .         C  � ()     " -     " 	          " .     %              " -     " -         "      " /         "      %               � 1)     +      " 0   u%              � L)         " 0   u%              � R)     � �                      �           �   l       ��                  �  �  �               0u�                    O   ����    e�          O   ����    R�          O   ����    ��          /   �  �      �                           3   �����	                                  3   �����	    ��                            ����                                            �           �   l       ��                  �  �  �               Hϕ                    O   ����    e�          O   ����    R�          O   ����    ��          /   �  �      �                           3   �����	                                  3   ����
    ��                            ����                                            ,          �   l       ���               �  �  �               �ϕ                    O   ����    e�          O   ����    R�          O   ����    ��      w	       �              �          �                    �          �                               �  A  �        �   ��         |  H
                                        
   
                   �  �           (
  8
           0
  @
         �            �   �          �    �  |  x
      4   ����x
                �                      ��                  �  �                  4Е                       �     �
                     �
                         � ߱            $  �  �  ���                                     �                      ��                  �  �                  �Е                       �    T  A  �        �   ��         �  �
                                        �
   �
                   @  4           �
  �
           �
  �
         �                          �  p  �  <         4   ����                                                 � ߱            $  �  �  ���                                             ,                         � ߱            $  �  �  ���                                     �                                           ��                            ����                                                  �           �   l       ��                 �  �  �               X̕                    O   ����    e�          O   ����    R�          O   ����    ��      x  $  �  �   ���                       8                         � ߱                      �          �  �      ��                 �  �  �              �v�                x     �        O   �    ��          O   �    ��          O   �    ��          p   �  X  �     �  8  h     d                x                      ��                  �  �                  w�                       �  �  �  /   �  �                                 3   ����p        �  �  �      �      4   �����      $   �    ���                       �  @         �              � ߱        �  �     �                �                      ��                  �  �                  hw�                       �  H     /   �  �                                 3   �����        �    ,      �      4   �����      $   �  X  ���                       4  @                        � ߱                   8                                      ��                  �  �                  �w�                       �  �  L  /   �  <                                 3   ����D  �  /   �  x     �                          3   ����d            �                      3   ����p  <    �  �  �      |      4   ����|      $   �    ���                       �  @         �              � ߱            /   �  h                                 3   �����      $  �  �  ���                       �                         � ߱                     $                                                             ��                            ����                                            �           �   l       ��                 �  �  �               `s                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       hX     
 !                   � ߱              �  (  �      �X      4   �����X                �                      ��                  �  �                  ��b                       �  8  �  �  �  Y            �  �  `      dY      4   ����dY                p                      ��                  �  �                  L�b                       �  �  �  o   �  !    ,                                 �  �   �  �Y      �  �   �  �Y      $  $  �  �  ���                       �Y     
 !                   � ߱        8  �   �  �Y      L  �   �  Z      `  �   �  <Z          $   �  �  ���                       lZ  @         XZ              � ߱                   !  T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����   !     ��                            ����                                            �           �   l       ��                   Y  �               ��b                    O   ����    e�          O   ����    R�          O   ����    ��      J$   "                   �          �  $  *    ���                       �Z     
 "                   � ߱                  �  �                      ��                   +  -                  �b                     +  4      4   �����Z      $  ,  �  ���                       ,[     
 "                   � ߱        �    .  4  D      @[      4   ����@[      /  /  p         "                      3   ����T[  �  �   J  `[          O   W  ��  ��  �[             "                  , �                          
                               � "     ��                            ����                                                        �   l       ��                  ;  B  �               ̕b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  H  U  �               |pb                    O   ����    e�          O   ����    R�          O   ����    ��           R  �   �       Tp      4   ����Tp      n   S     �          �p        T    ,      �p      4   �����p      �   T  �p    ��                            ����                                                      �   l       ��                  [  k  �               tqb                    O   ����    e�          O   ����    R�          O   ����    ��      �p  �          �p  �          �p  �          �p  �              � ߱        �  Z   e  �    �        �p                  �               �              �              �              �              � 	             � 
             � ߱        �  h   g  0   �        �p              H  s   i                 @          �  4  �       ��                            7   ����           ��                tq   �            �                  6   i         �   ��               tq   �            �                                                                X  L           4q  Dq  Tq  dq           <q  Lq  \q  lq                         0          �q  r  r   r  ,r              �         ��                            7   ����          ��               �r   �            \                  6   i        �   ��         �  �r   �            \                                                        8r   Dr  	 Pr                 �  �           \r  lr  |r           dr  tr  �r         �   
        
 �   �          �r  �r  s                 q   q   q   (q    l        
   j  �� d             s    ��                              ��        �                  ����                            \                         ��                    �           �   l       ��                  q  {  �               @b                    O   ����    e�          O   ����    R�          O   ����    ��      �     x   s  }          O   y  ��  ��  4s    ��                            ����                                            �           �   l       ��<"          	     �  �  �                Ab                    O   ����    e�          O   ����    R�          O   ����    ��      l  A   �      %    ��                                                      Hs                 X  L           Ts           \s         �            ,   <    �    �  �  �      ds      4   ����ds      O   �  ��  ��  ps  h  B  �       (    ��                                                       |s                 T  H                        �s                      (   8    �    �  �         �s      4   �����s                                      ��                  �  �                  ��a                       �  �  T  	  �  D                                        3   �����s      O   �  ��  ��  �s  �s      '               �s      '                   � ߱        (  $  �  l  ���                       �s      &               �s      &                   � ߱        T  $  �  �  ���                       (  	  �  �                         xt        �  3   �����s  �  3   �����s  �  3   ���� t  �  3   ����t  �  3   ���� t  �  3   ����,t  �  3   ����8t    3   ����Lt    3   ����Xt      3   ����lt  �  V   �  T  ���                               & 	       	             ߱                    <    �  �  �      �t      4   �����t      O   �  ��  ��  �t                |  D"          L      ��           	     �  �  d              PHa                �      �  �      O   �     ��  �t      O   �     ��  �t  �  F  �              ��                                                    �    �  �  d      �t      4   �����t                t                      ��                  �  �                  �Ha                       �  �  �  	  �  �                                        3   �����t      O   �     ��  �t  �t      &                   � ߱        �  $  �  �  ���                                     �                      ��                R  X  �              ,Ka                �
     R  (      O   R    e�       
    S  �  T	       u      4   ���� u                d	                      ��                  S  V                  4a                       S  �  �	  $  T  �	  ���                       u      '                   � ߱              U  �	  �	      ,u      4   ����,u      O   U  �� ��          A  W      # h
   ��         P
  �u                                        Tu   `u   lu                 �
  �
           xu  �u  �u           �u  �u  �u         �            �
   �
        Y  �
         �u      4   �����u      O   Y     ��  0v  $    a  4  �      <v      4   ����<v                �                      ��                  a  h                  p5a                       a  D        b  �  X      dv      4   ����dv                h                      ��                  b  g                  �5a                       b  �    	  c  �                                    �  3   �����v  �  3   �����v  �  3   �����v  �  3   �����v  �  3   �����v  �  3   �����v      3   ����w      O   f     ��  w  P    j  @  �      w      4   ����w                �                      ��             	     j  p                  �6a                       j  P        l  �  d      Dw      4   ����Dw  	              t                      ��             	     l  o                  7a                       l  �        m  �  �  �  �w      4   �����w      V   m  �  ���                        �w      #                   � ߱            V   n  $  ���                        �w      #                   � ߱        `  9   x     �  �   y    P  |  n       �w                     �w       1       1       �w                      x       0       0       x                     Tx       '       '       `x       (       (       lx                     xx                    �x                     �x       )       )       �x                     �x                    �x                    �x       %       %       �x                         � ߱            V   y  �  ���                        
  L L L      O O O      * * *              
 
 
      g g g                              D D D      B B B      ! ! !      = = =              C C C      " " "      i i i      h h h      k k k      j j j      t t t              c c c      4 4 4      @ @ @      H H H      ; ; ;              a a a      ] ] ]      - - -      . . .      G G G      : : :              E E E      P P P      / / /      5 5 5      + + +      , , ,      3 3 3      & & &      f f f                      K K K              e e e              I I I              2 2 2      # # #      d d d      J J J      $ $ $      T T T      U U U      V V V      W W W      X X X      Y Y Y      Z Z Z      [ [ [      \ \ \      ^ ^ ^      _ _ _      ` ` `      l l l              F F F      v v v      y y y      | | |      R R R              ? ? ?      M M M      > > >                      < < <      u u u      z z z      7 7 7              Q Q Q      	 	 	      S S S   
  N N N                 6 6 6      x x x      { { {      p p p      q q q      } } }      s s s      o o o      r r r      ~ ~ ~      n n n      m m m      w w w              8 8 8      b b b      9 9 9      A A A   l  A  �       )    ��         �  4y                                        �x   y                   X  L           y  $y           y  ,y         �            $   8        �  �  �      dy      4   ����dy      V   �  �  ���                        ly       O       O           � ߱        xy      #                   � ߱        H  V   �  �  ���                        X  9   �  *   @  �   �   * 0  \         �y      *               �y      *               �y     *               �y     *               �y     *               �y     *                   � ߱            V   �  x  ���                                           1                i % %      h $ $      k ' '      j & &              K         
 
      e ! !              #        d          R                7 	 	      Q       A   �      + �  	 ��        	 �  Tz                                          z   z   z                    �      	     $z  4z  Dz      	     ,z  <z  Lz         �            �   �    �    �  0  �  �  �z      4   �����z  �z      *               �z     *                   � ߱            V   �  @  ���                        H{      *               \{     *                   � ߱            V   �  �  ���                        p{                    �{                    8|                    D|                        � ߱        �  V   �  (  ���                        �    �  �  H  �  P|      4   ����P|  x|      '               �|      '                   � ߱            $  �     ���                       �|      '               �|      '                   � ߱            $  �  t  ���                       D  /   �       $                          3   �����|  T        D                      3   �����|  �        t                      3   �����|  �        �                      3   ����}  �        �                      3   ����}                                3   ���� }            4                      3   ����,}  �    �  `  p      8}      4   ����8}      O   �     ��  P}  �    �  �  �      \}      4   ����\}      8  �           �  �  �      d}      4   ����d}      8  �     <     �     ,       l}      4   ����l}      8  �     x     �  X   h       t}      4   ����t}      8  �  *         �  �   �       |}      4   ����|}      8  �  #       	  �  �                                         3   �����}              & 	 `!                                             ' 
 �!          �!  �!   h l!                                                                                          (   8   H   X          (   8   H   X   ��     & '   ��                            ����                            T"  + 	 \"  =   �  *   l"  )      =   �     t"  #  |"  (      %                  �           �   l       ��H               �  �  �               �Ua                    O   ����    e�          O   ����    R�          O   ����    ��      �  A  �       %    ��        
                                              �}                 X  L           �}           �}         �            ,   <                  �                      ��                  �  �                  �a                0     �  l  �  $  �    ���                       �}      ,                   � ߱              �      �          P  8      ��                  �  �  h              x�a                �     �  @      �  8       ��                            7   ����    #     
 ��                ~    �            �                  6   �       # �  
 ��         �   ~    �            �                                                        �}   �}   �}   �}                   $        
     �}   ~  ~      
     �}  ~  ~                      �            O   ����  e�          O   ����  R�          O   ����  ��            �  �  �    �~      4   �����~      $  �  �  ���                       �~      ,                   � ߱            $  �  0  ���                       �~      ,                   � ߱        ,  @                   8      
                   � ߱        �  $   �  \  ���                       �  A  �       # @   ��            �                                        h   t   �   �                   �  �           �  �  �           �  �  �         �            \   t          �  �  �      <�      4   ����<�      $  �    ���                       D�      
                   � ߱            /   �  \                                3   ������             ,  �          �  �    �                                        ,     ��                              ��        �                   ��                            ����                            P  #      %                        x  �   l   �  ��                 �  7  �               (�d                    O   ����    e�          O   ����    R�          O   ����    ��      �(   -    �              �          �(   -                 �          �(   -    8                      �(   -    `             ,         �(   -    �             T         �(   -                   |                       P  h                 ��                  5  8              T�b                         �      O        ��  ��      O        ��  ��  $  A         . �   ��         �  �                                         ��   Ā   Ѐ                              ܀  �  ��           �  �  �         �            �   �    X      @  �      X�      4   ����X�                �                      ��                                      @�b                         P  @  	                                           3   ����d�     3   ����p�  0  3   ����|�      3   ������      O     ��  ��  ��  �  9     /   ��      /               ��      /               ��      / 	       	       ā      /               Ё      /               ܁      /               �      /               �      / 
       
        �      /               �      /               $�     /               0�      /                   � ߱        �  V     h  ���                        �    (    L  �  <�      4   ����<�  d�     /                   � ߱            V   )     ���                        p�     /                   � ߱            V   *  x  ���                        |�                        � ߱        (  V   ,  �  ���                        �    .  D  �      ��      4   ������  Ă                     Ђ       %       %           � ߱            V   /  T  ���                        �  8  3  .       8  4  /               -                                            - .   ��                            ����                                =   5  /       8   7  .       8   7  .       ��          \  h   ��  e  `          �          
 �                                                                    A     �         �)                                    
 �                                                                   H     �  	     ��)                                    
 �                                                                   O     �  
       �)                                    
 �                                                                   K    �  
       *                                    
 �                                                                    �     �  
       *                                    
 �                                                                   �     �         #*                                    
 �                                                                   �     �         1*                                      �                                                                                                                                                                                   �   d d     �   ���(�	�(  � �                                               �                                                                        d     D                                                                 \  ���p                                            0       >*                @      P   �� ?Q                                                           Q*  G   
 X  �� �Q                                                        !     �      \  �� �p                                                  e*                @      P    fQ                                                           m*  G   
 X   pQ                                                        0     �      P   �
�	Q                                                           t*  E     p  �
��X                                                             �                
           
 X  H��Q         d   x                                         ?     �  
    H  ,I��                                \          �           P ����2                                          $           �       P ��d �2         (  <                                          �       H  , 4!	                                 N                    H  ,�4!� 
                                 U          "           D                                                                    TXS appSrvUtils B-Adelantos CcbCDocu B-Facturas B-NC DOCU CodCia CodDoc NroDoc FchDoc CodCli NomCli DirCli RucCli CodAnt CodPed NroPed NroOrd ImpBrt ImpExo PorIgv ImpIgv ImpDto ImpTot SdoAct FlgEst usuario UsrDscto FlgCie FchCie HorCie CodMon TpoCmb CodAlm LugEnt Tipo CodMov CodVen ImpIsc ImpVta ImpFle FchCan Glosa CodRef NroRef FchVto CodCob CodCta CodAge FlgUbi FlgUbiA FchUbi FchUbiA FlgSit Cndcre CodDiv ImpInt FmaPgo FchAct FlgSitA TipVta PorDto TpoFac FchCbd NroSal FlgCbd Codope NroMes Nroast FchAnu UsuAnu CodDpto CodProv CodDist FlgCon LugEnt2 FlgAte FchAte imptot2 ImpCto AcuBon NroCard TipBon CCo FlgEnv puntos mrguti Sede Libre_c01 Libre_c02 Libre_c03 Libre_c04 Libre_c05 Libre_d01 Libre_d02 Libre_f01 Libre_f02 FchCre Libre_f03 Libre_f04 Libre_f05 FchCobranza UsrCobranza DivOri ImpPro ImpDto2 GlosaImpDto2 CodCaja Dcto_Otros_Mot Dcto_Otros_Factor Dcto_Otros_VV Dcto_Otros_PV Lista_de_Precios TotalValorVentaNetoOpGravadas TotalValorVentaNetoOpGratuitas TotalTributosOpeGratuitas TotalIGV TotalImpuestos TotalValorVenta TotalPrecioVenta DescuentosGlobales PorcentajeDsctoGlobal MontoBaseDescuentoGlobal TotalValorVentaNetoOpNoGravada TotalDocumentoAnticipo MontoBaseDsctoGlobalAnticipo PorcentajeDsctoGlobalAnticipo TotalDsctoGlobalesAnticipo MontoBaseICBPER TotalMontoICBPER TotalValorVentaNetoOpExoneradas TotalVenta ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST s-codcia cl-codcia s-coddiv s-user-id s-coddoc A/C cCodDoc N/C x-Moneda wWin BUTTON-1 BUTTON-2 COMBO-NroSer FILL-IN-CodCli FILL-IN-NomCli FILL-IN-NroDoc RECT-2 RECT-3 BROWSE-2 SELECCIONE EL ANTICIPO DE CAMPA�A A ASIGNAR x(3) X(9) 99/99/9999 x(10) ->>,>>>,>>9.99 fMain X(11) X(256) X(3) XXX-XXXXXX Comprobante a Generar Filtros GUI CANJE DE ANTICIPOS DE CAMPA�A POR NOTA DE CREDITO input-var-1 input-var-2 input-var-3 output-var-1 output-var-2 output-var-3 HANDLE-CAMPO BUTTON-LOOKUP PARIENTE load-imagen program_name program_call titulo-look  ENTRY img/b-lookup FRAME,WINDOW FILL-IN ENTRY CHOOSE INTEGER DECIMAL corre_calculadora DATE corre_calendario BROWSE KEYPRESS qbusca ADM-BUSCA qimprime ADM-IMPRIME campo_name PF-G005 Descripci�n de Campos * _BUSCA-LOOKUP OK-SET-WAIT-STATE GENERAL ? _CORRE-PROGRAM DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   BUTTON-2 FILL-IN-CodCli BUTTON-1 COMBO-NroSer BROWSE-2 RECT-2 RECT-3 CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE P ADM-ERROR VALUE-CHANGED CHOOSE Tab FacCorre Correlativos por documento 999 999999 gn-clie Maestro de Clientes Clientes iStartPage ADM-ERROR ADM-CREATE-OBJECTS DISABLE_UI ENABLE_UI EXITOBJECT FacCfgGn Configuracion General x-Saldo-Actual x-Monto-Aplicar x-Importe-NC x-Saldo-Adelanto x-Saldo i s-NroSer x-TpoCmb-Compra x-TpoCmb-Venta x-ImpMn x-ImpMe Gn-tccja NO se ha registrado el T.C. de Caja rpta Se va a generar la Nota de Cr�dito Confirme la generaci�n No se pudo bloquear el adelanto LocalCounter Se ha llegado al l�mite del correlativo: No se puede generar el documento serie N gn-ven Vendedores CcbDDocu 00001 CcbTabla Tabla de cobranzas Proceso Terminado GENERA-NC cListItems , INITIALIZEOBJECT para_CodDoc para_NroDoc para_NroDocCja para_TpoCmb para_ImpNac para_ImpUSA B-CDocu DOCUMENTO NO REGISTRADO CCBDMOV HH:MM:SS C PROC_APLICADOC-2 iCodMon SOLES DOLARES FMONEDA llave00 llave01 llave02 llave03 llave04 llave05 llave06 llave07 llave08 llave09 llave10 llave11 llave12 llave13 llave14 Llave15 Llave16 Doc. Numero Fecha de Emisi�n Fecha de Vencimiento Moneda Importe Total Saldo Actual GENERAR N/ CREDITO C�digo del Cliente: FILTRAR Nombre Seleccione la serie de la N/C IDX01 Llave01 idx01 d  P8  �  `?      ( �    H                                                  �                                             T   �                                         e  h  �   �                                         k  l  m  n  o  p  �   <                                        s  t  u  v    |                                        �  �  L  �                    �                  adm-busca   �  �  �                      �                  adm-imprime �  �  0        $        campo_name  X        H        program_call              p        program_name    �  �     	             �                  _busca-lookup   �  �  �  �  �  �  �  �  �  �  �                  OK-SET-WAIT-STATE   �  T     
   �          D                  _corre-program  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��      �        pcProp      ��      �        pcProp      ��              plCancel    @  ��      4        pcProcName  d  ��      X       
 pcProcName  �  ��      |        pcProcName      ��      �       
 pcProcName      ��      �        piPageNum       ��      �        piPageNum       ��              pcPageList      ��      0        pcProc  \  ��      P        pcLinkName      ��      t        pcLinkName  �  ��      �       
 phTarget        ��      �        phTarget        ��      �        piPageNum       ��              pcValue     ��      $        piPageNum       ��      H        pcAction        ��      l       
 phAppService        ��      �        pcMode  �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    (  ��               pcText  H  ��      @        pcText      ��      `        pcText  �  ��      �       
 phObject    �  ��      �       
 phObject        ��      �        phObject        ��      �        pcField     ��              pcCursor    <  ��      0       
 phCaller    `  ��      T        phCaller    �  ��      x        phCaller        ��      �        phCaller    �  ��      �        pcMod   �  ��      �        pcMod       ��       	       
 pcMod   ,	  ��       	       
 phSource    P	  ��      D	        phSource        ��      h	       
 phSource    �	  ��      �	        pdRow       ��      �	        pdRow       ��      �	       
 hTarget �	  ��      �	        pcMessage       ��      
        pcMessage       ��      4
        plEnabled              T
     cType     �
     @   @
          �
                  getObjectType   �
  �
  �
  �
  !      �
  
   hReposBuffer    �
  !      �
  
   hPropTable    !         
   hBuffer     !        
   hTable  \
  d     A   �
          T                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �      "      �  
   hProc       "      �        pcProcName  $  ,  	   B   �  �                        start-super-proc    *  +  ,  -  .  /  J  W  Y  �  �     C                                   %  P  �     D                                   )  *  �  �     E                                   -  .  �  $     F                                   l  �  X     G                                   n  o  (  �     H                                   z  {  |  `  �     I                                   �  �        J                                   �  �  �  �  <     K                                   �  �  �  �  �  �  �  �    �     L                                   �  �  \  �     M                                   �  �  �  �  �  �  �       N                                   �  �  �  �  �  L     O                                   �  �  �  �    �     P                                           \  �     Q                                       �       R                                 adm-create-objects  B  �  X     S               L                  disable_UI  R  S  T  U    �     T               �                  enable_UI   e  g  i  j  k  h  �     U               �                  exitObject  x  y  {  $  &           x-Saldo-Actual  H  &      8     x-Monto-Aplicar l  &      \     x-Importe-NC    �  &      �     x-Saldo-Adelanto    �  &      �     x-Saldo �  &      �     i   �  &      �     s-NroSer      '     �     x-TpoCmb-Compra 0  '           x-TpoCmb-Venta  L  '     D     x-ImpMn h  '     `     x-ImpMe �  &   	   |     rpta        '     �     LocalCounter    �  �  ?   V              �                  Genera-NC   �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  R  S  T  U  V  W  X  Y  a  b  c  f  g  h  j  l  m  n  o  p  x  y  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �      ,      �     cListItems  �  D     W   �          0                  initializeObject    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  -      �        para_CodDoc �  -      �        para_NroDoc �  -      �        para_NroDocCja    -              para_TpoCmb 4  -      (        para_ImpNac     -      L        para_ImpUSA      .  C  h  B-CDocu    �     X       |  X  �                  proc_AplicaDoc-2                    (  )  *  ,  .  /  3  4  5  7      0              iCodMon p  T     Y       �      L                  fMoneda F  G  I  K    (+      $ $"      �)                          �  �  ~   DOCU    �         �         �         �         �         �         �         �         �         �         �         �         �         �                                             $         ,         4         <         D         L         T         \         d         l         t         |         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                             $         ,         4         <         D         L         X         `         h         p         x         �         �         �         �         �         �         �         �         �         �         �         �         �         �  
      �         �  
                                                    $         0         <         H         T         `         l         x         �         �         �         �         �         �         �         �         �         �         �         �                             (          8          H          \          |          �          �          �          �          �          �          !         $!         @!         `!         x!         �!         �!         �!         �!         �!         "         CodCia  CodDoc  NroDoc  FchDoc  CodCli  NomCli  DirCli  RucCli  CodAnt  CodPed  NroPed  NroOrd  ImpBrt  ImpExo  PorIgv  ImpIgv  ImpDto  ImpTot  SdoAct  FlgEst  CodCob  CodCta  usuario FlgCie  FchCie  HorCie  CodMon  TpoCmb  CodAlm  LugEnt  Tipo    CodMov  CodVen  ImpIsc  ImpVta  FchCan  Glosa   CodRef  NroRef  FchVto  CodAge  FlgUbi  FlgUbiA FchUbi  FchUbiA FlgSit  Cndcre  CodDiv  ImpInt  FmaPgo  FchAct  FlgSitA TipVta  PorDto  TpoFac  UsrDscto    FlgCbd  FchCbd  NroSal  Codope  NroMes  Nroast  FchAnu  UsuAnu  CodDpto CodProv CodDist FlgCon  LugEnt2 FlgAte  FchAte  ImpFle  imptot2 ImpCto  AcuBon  NroCard TipBon  CCo FlgEnv  puntos  mrguti  Sede    Libre_c01   Libre_c02   Libre_c03   Libre_c04   Libre_c05   Libre_d01   Libre_d02   Libre_f01   Libre_f02   FchCre  Libre_f03   Libre_f04   Libre_f05   FchCobranza UsrCobranza DivOri  ImpPro  ImpDto2 GlosaImpDto2    CodCaja Dcto_Otros_Mot  Dcto_Otros_Factor   Dcto_Otros_VV   Dcto_Otros_PV   Lista_de_Precios    TotalValorVentaNetoOpGravadas   TotalValorVentaNetoOpGratuitas  TotalTributosOpeGratuitas   TotalIGV    TotalImpuestos  TotalValorVenta TotalPrecioVenta    DescuentosGlobales  PorcentajeDsctoGlobal   MontoBaseDescuentoGlobal    TotalValorVentaNetoOpNoGravada  TotalDocumentoAnticipo  MontoBaseDsctoGlobalAnticipo    PorcentajeDsctoGlobalAnticipo   TotalDsctoGlobalesAnticipo  MontoBaseICBPER TotalMontoICBPER    TotalValorVentaNetoOpExoneradas TotalVenta  D"          8"  
   appSrvUtils d"        X"     s-codcia    �"        x"     cl-codcia   �"        �"     s-coddiv    �"  	 	     �"     s-user-id   �"       �"     s-coddoc     #       �"     cCodDoc  #       #     x-Moneda    <#  
 
    4#  
   wWin    `#  
 
    P#     COMBO-NroSer    �#  
 
    t#     FILL-IN-CodCli  �#  
 
    �#     FILL-IN-NomCli  �#  
 
    �#     FILL-IN-NroDoc  �#       �#     input-var-1 $        $     input-var-2 ,$        $     input-var-3 P$       @$     output-var-1    t$       d$     output-var-2    �$       �$     output-var-3    �$       �$  
   HANDLE-CAMPO    �$       �$  
   BUTTON-LOOKUP    %       �$  
   PARIENTE     %       %     load-imagen D%       4%     program_name    h%       X%     program_call    �%       |%     titulo-look �%        �%  
   gshAstraAppserver   �%        �%  
   gshSessionManager   �%        �%  
   gshRIManager    $&        &  
   gshSecurityManager  L&        8&  
   gshProfileManager   x&        `&  
   gshRepositoryManager    �&        �&  
   gshTranslationManager   �&        �&  
   gshWebManager   �&        �&     gscSessionId    '         '     gsdSessionObj   4'        $'  
   gshFinManager   X'        H'  
   gshGenManager   |'        l'  
   gshAgnManager   �'        �'     gsdTempUniqueID �'        �'     gsdUserObj  �'        �'     gsdRenderTypeObj    (        �'     gsdSessionScopeObj  ,(  
 
    $(  
   ghProp  L(  
 
    @(  
   ghADMProps  p(  
 
 	   `(  
   ghADMPropsBuf   �(  
 
 
   �(     glADMLoadFromRepos  �(  
 
    �(     glADMOk �(  
 
    �(  
   ghContainer �(  
 
    �(     cObjectName )  
 
    )     iStart  0)  
 
    $)     cAppService P)  
 
    D)     cASDivision |)  
 
    d)     cServerOperatingMode    �)  
 
    �)     cFields     
 
    �)     iStartPage  �)    �C  �)  B-Adelantos �)     C  �)  B-Facturas  *    C   *  B-NC     *    �  *  DOCU    <*       0*  CcbCDocu    T*       L*  PF-G005 p*   #    d*  FacCorre    �*   $    �*  gn-clie �*    %    �*  FacCfgGn    �*  ! (    �*  Gn-tccja    �*  " )    �*  gn-ven  �*  # *   �*  CcbDDocu    +  $ +    +  CcbTabla        % /    +  CCBDMOV          D   �  �  �  �  �  	  d  j  r  �           #  $  &  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   
  "
  #
  $
  %
  &
  �
                                  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  /  ;  <  ?  @  A  B  D  E  G  H  I  J  K  L  M  N  P  Q  R  S  T  U  W  X  Y  Z  [  \  ]  ^  _  `  a  b  c  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z  {  |  �  �  2  3  <  =  A  B  C  E  H  R  n  �  �  �  �  �  `  x  y  �  �  �  �  �  �  �  �  �  �  �    �  �  �  �  "  #  $  &  (  ,  E  F  G  I  Q  W  ]  `  e  i  j  k  m  q  �    <  =  i  v  �  �  �  �  �  �  �            !  %  )  *  +  ,  .  0      �� & d:\newsie\on_in_co\APLIC\vtagn\i-faccorre-01.i   l/  �� ' d:\newsie\on_in_co\APLIC\lib\lock.i  �/  H� % C:\Progress\OpenEdge\src\adm2\windowmn.i �/  f!  C:\Progress\OpenEdge\src\adm2\containr.i 0  � $ %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    @0  ��  C:\Progress\OpenEdge\src\adm2\visual.i   �0  # # %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �0  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    �0  �� " %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   01  I�  C:\Progress\OpenEdge\src\adm2\smart.i    t1  Ds ! C:\Progress\OpenEdge\gui\fn  �1  tw   %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �1  Q.  C:\Progress\OpenEdge\gui\set 2  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i 82  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    l2  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �2  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �2  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i (3  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i h3  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �3  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �3  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i     4  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i d4  �j  C:\Progress\OpenEdge\gui\get �4  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �4  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    5  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i H5  Su  C:\Progress\OpenEdge\src\adm2\globals.i  |5  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �5  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �5  �  C:\Progress\OpenEdge\src\adm2\appsprto.i 46  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   h6  �X 
 C:\Progress\OpenEdge\src\adm2\visprto.i  �6  !� 	 %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �6  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i (7  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    \7  ��  %d:\newsie\on_in_co\src\adm-vm\method\vmviewer.i  �7  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �7  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   8  QW   d:\newsie\on_in_co\APLIC\vta2\wcanjeadelxnc-copia.w      r  w      �8  V   `  &   �8       '   �8     �  &   �8  �  3      �8     �  %   �8    '      �8  �          9     �     9  �   �      9     �     09  �   �     @9     v  $   P9  �   `     `9     ^  !   p9  �   W     �9     U  !   �9  �   T     �9     R  !   �9  r   6     �9  n        �9     �  #   �9  i   �     �9     �      :  P   �     :  �   }      :     %  "   0:  �         @:     �     P:  �   �     `:     �     p:  �   �     �:     �     �:  g   �     �:     ~     �:  O   f     �:  �   �     �:     �  !   �:  �   �     �:     f       ;  �   [     ;     9      ;  �   8     0;          @;  �        P;     �     `;  �   �     p;     �     �;  �   �     �;     �     �;  �   �     �;     x     �;  }   l     �;     J     �;     �     �;     �      <     1     <  7   �      <  �   �     0<  O   �     @<     �     P<     �     `<  �   8     p<  �   /     �<  O   !     �<          �<     �     �<  �   �     �<  x   �     �<  M   �     �<     o     �<     #      =  a        =  �  �
      =     �
     0=  �  �
     @=  O   �
     P=     z
     `=     ,
     p=  �   V	     �=     (     �=     }     �=  x   w     �=     ^     �=     �     �=     �     �=     �     �=     �      >  Q   �     >     J      >          0>           @>     �     P>  f   �     `>     Z  
   p>  "        �>       	   �>     �     �>  Z   �     �>     �     �>     Y     �>     E     �>     +     �>     �      ?    �      ?     �      ?  4   �       0?     M      @?     !       P?           