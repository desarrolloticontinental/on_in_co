	��V+%P�5  �              /                                � 35BC010Cutf-8 MAIN O:\on_in_co\APLIC\vta\r-rep010.w,, PROCEDURE PrecioListaMinorista,,INPUT S-CODDIV CHARACTER,INPUT S-CODMON INTEGER,OUTPUT S-UNDVTA CHARACTER,OUTPUT f-Factor DECIMAL,INPUT S-CODMAT CHARACTER,INPUT X-CANPED DECIMAL,INPUT x-NroDec INTEGER,INPUT s-FlgSit CHARACTER,OUTPUT F-PREBAS DECIMAL PROCEDURE initializeObject,, PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE Carga-temporal,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER      �5              �	             l� �5  |�              n              �+  	  +   x �  7   �| `  8   � �   C   �� D  D   @� |  E   �� `  F   � $  G   @� �  H   ؚ   I           � �  ? �� �!  iSO8859-1                                                                           �4   # �                                      �                  �   	                 (,     \,   ��   ��  �4         (�  �   H5      T5          �                                             PROGRESS                         �           
    
                    �              �                                                                                                     
                                                                                                         �             �         �       �  L  4     (4  �  �      T4         �             `.          x/      �   ,  �
      �  
    
                  �  \                                                                                                       �
          
  �  �
      T  
    
                  @               �                                                                                          �
          
  �  �
         
    
                  �  �             p                                                                                          �
          
  0  �
      �  
    
                  �  `                                                                                                       �
          
  �  �
      X  
    
                  D    	           �                                                                                          �
          
  �  �
        
    
                  �  �  
           t                                                                                          �
          
  4  �
      �  
    
                  �  d                                                                                                        �
          
  �        \  
    
                  H               �                                                                                                    
  �                                  �  �             x                                                                                                       8	  -      �                        �  h	             $	                                                                                          -            �	  ;      `	  
    
                  L	  
             �	                                                                                          ;          
  �
  I      
  
    
                  �	  �
             |
                                                                                          I          
  <  W      �
  
    
                  �
  l             (                                                                                          W          
  �  e      d                        P               �                                                                                          e            �  u                              �  �             �                                                                                          u            @  �      �                        �  p             ,                                                                                          �                �      h                        T  <             �                                                                                          �                         INTEGRAL                         PROGRESS                         �     �  �      �                         i�&N            �  �                              �  �                         �  3!     CODDIVDESDIVCODCIADIRDIVTELDIVFAXDIVRESPONFLGREPFLGPREUNIFLGAPRCOTFLGAPRPEDFLGPREVTADIASVTOCOTDIASVTOPEDDIASVTOO_DDIASAMPCOTTIPDTOMODPREUNIFLGEMPAQUEFLGROTACIONCANALVENTAFLGPICKINGFLGBARRASFLGMINVENTAFLGDTOPROMFLGDTOVOLFLGTARJETAVENTAMOSTRADORVENTAMAYORISTAVENTAMINORISTAFLGDTOCLFCLIPORDTOCLFCLIFLGDTOCNDVTA                                                                       	          
                                                                                                                                                                                                                                       !          "          p     �  �      �                         �P            �  �                              �                        �    �e     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2                                                                       	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          L        �                                i�&N               ��                              �  �                      �     �       CODCIACODDOCNRODOCNROITMUNDVTACODMATPREUNIPORDTOIMPDTOIMPLINCANDESAFTIGVAFTISCPREBASPREVTAIMPIGVIMPISCFACTORCANDEVPORDTO2PESMATCODCLIALMDESPOR_DSCTOSFLG_FACTORFCHDOCCODDIVIMPCTOPUNTOSMRGUTIIMPPROIMPDTO2                                                                        	          
                                                                                                                                                                                                                                     !          X&     (   �      (                          �9�O            1   ��                              �  �                      X   �  |�     CODMATDESMATCODMARUNDSTKUNDCMPFACEQUCODCTACODNEWMONVTAPREVTAPREBASAFTIGVVINMN1CODCIAVINMN2CODFAMVCTMN1FCHACTCODPR1CODPR2VCTMN2ARTPROFCHUSALFCHUCMPPMAXMN1PMAXMN2PULTMN1PULTMN2USUARIOFCHINGFCHCESFCHALZCLFMATUNDBASSUBFAMCODBRRCODANTTIPARTFCHPRMDFCHPRMHFCHREAPESMATDETALLECANEMPALMACENESDESMARAFTISCPORISCPORVTATPOMRGCTOLISCTOPRMMRGUTIPORMAXFCHMPREUNDANTPREANTPREACTDSCTOSTPOPROPORIGVCTOTOTTPOSUMCTOUNDORDENORDLISORDTMPTPOARTTPOCMBPPCHR__01CHR__02CHR__03DEC__01DEC__02DEC__03DATE__01DATE__02DATE__03MRGUTI-AMRGUTI-BMRGUTI-CPREOFIUNDAUNDBUNDCFLGINTFLGPRECLASEFCHPROMCATCONTATIPROTDSCTOPROMINFORFLGINFORPROMDIVIPROMFCHDPROMFCHHPROMDTODTOVOLRDTOVOLDUNDALTDSCALTMRGALTPREALTLICENCIAPROMMINDIVIPROMMINFCHDPROMMINFCHHPROMMINDTOCODDIGESAVTODIGESALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKMINSTKMAXSTKREPDESCRIPCION-LARGADESCRIPCION-TECNICASW-WEBWEB-SUBCATEGORIA                                                                      	          
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
        p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          �(      �   �      �                          ;�1O            �   �w                              �  �&                      �'  �&  �      CODCIACODMATMONVTATPOCMBFCHACTCANEMPUSUARIOFCHINGFCHCESFCHREACHR__01PREOFIDEC__01DESMATDESMARCODFAMSUBFAMFCHPRMDFCHPRMHPROMDIVIPROMFCHDPROMFCHHPROMDTODTOVOLRDTOVOLD                                                                      	          
                                                                                                               
         
         
         
          
        ! 
        �)  !   �   �      �                          i�&N            �   7-                              �  8)                      x)  H)  /      CODUNIDCODALTERDESALTEREQUIVALCODANTERMULTIPLOS                                                                 "   !  �      !                         ;�1O            "!  ˻                              �  @*                      �*  P*  �      CODCIACODDIVCODMATMONVTATPOCMBFCHACTCANEMPUSUARIOFCHINGFCHCESFCHREACHR__01PREOFIDEC__01PROMFCHDPROMFCHHPROMDTODTOVOLRDTOVOLDDESMATDESMARCODFAMSUBFAMPROMFCHHORDPROMFCHHORH                                                                        	          
                                                                                           
         
                                                   "          "                     x�                                                ��          �-  .  P ��,            
                Todas                                                       
             
             
                                         
                                                                                                                P   `   p   �   �   �   �   �   �   �   �           0  @  P  `  p      P   `   p   �   �   �   �   �   �   �   �          0  @  P  `  p                                                                                                                                     	                  
                                                                                                         �1  �1  �1  �1                               2  2  2  $2  2                         (2  02  82  H2  @2                         L2  T2  `2  p2  h2                         x2  �2  �2  �2  �2                         �2  �2  �2  �2                              �2  �2  �2  �2                              �2   3  3  3                              3  $3  43  L3                              P3  X3  `3  x3                              |3  �3  �3  �3                              �3  �3  �3  �3                              �3  �3  �3  4                                                                          coddiv  x(5)    Divisi�n        coddoc  x(3)    Documento   Codigo      nrodoc  X(9)    N�mero  Numero      fchdoc  99/99/9999  Emisi�n Fecha   TODAY   codmat  X(6)    Producto    Codigo Articulo     desmat  x(60)   Descripci�n     prevta  >>>,>>>,>>9.99  Unitario de Venta   0   undvta  x(6)    Unidad de Venta     prebas  >>>,>>>,>>9.99  Unitario Lista Precios  0   undbas  x(6)    Unidad Lista Precios        univta  >>>,>>>,>>9.99  Unitario Absoluto Venta 0   unibas  >>>,>>>,>>9.99  Unitario Absoluto Lista 0   undstk  x(5)    Unidad Absoluta     �  ���������   �         �     O!                �     i     	    �  �  �  �  �  �  �  �  �  �  �  �  �    ��                                               �          ����                            O!         �!   ��    �!   YE    �!   ��    �!   ��    �!    �    �!  ! ��    �!  " �4    undefined                                                               �       ��  �   l   ��    ��                  �����               �_[                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     7          assignFocusedWidget         �      �            LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    $       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    6       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          L       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    X       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    d       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    w       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    #      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    7      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    E      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    U      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    f      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    s      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    �    ]  �
  �
  P  x       4   ����x       o   ^       �
                              �  �   NA  �   �  �   �  �      �      �         $    8    L    `  `  t  
`  �  $  �    �     �      $  o  |  ���                       �     
                    � ߱        ؁    �  �  @      �      4   �����                P                      ��                  �  �                  T`                       �  �  �    �  l  |            4   ����      $  �  �  ���                       d  @         P              � ߱              �  �         �      4   �����      $  �  ,  ���                       �  @         �              � ߱        assignPageProperty                              �  �      ��                  &  )                �a                    O   ����    e�          O   ����    R�          O   ����    ��            ��   T                             ��                  H           ��                            ����                            changePage                              @  (      ��                  +  ,  X              �a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             @  (      ��                  .  0  X              �a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  p           ��                            ����                            constructObject                             l  T      ��                  2  7  �              �aa                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
  �             �  
             ��                 �               �� 
                   
         ��                            ����                            createObjects                                 �      ��                  9  :  (              �_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                                �      ��                  <  >  (              _                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            destroyObject                               <  $      ��                  @  A  T              ��`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                <  $      ��                  C  E  T              H_                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            initializeObject                                l  T      ��                  G  H  �              X _                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               |  d      ��                  J  K  �              _                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               |  d      ��                  M  O  �              X_                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            notifyPage                              �  �      ��                  Q  S  �              �$_                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            passThrough                             �  �      ��                  U  X  �              Й_                    O   ����    e�          O   ����    R�          O   ����    ��            ��   0             �               ��                  $           ��                            ����                            removePageNTarget                               $        ��                  Z  ]  <              �a                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             T  
             ��                  |           ��                            ����                            selectPage                              t  \      ��                  _  a  �              �;`                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            toolbar                             �  �      ��                  c  e  �               �`                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            viewObject                              �   �       ��                  g  h  �               �_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                �!  �!      ��                  j  l  �!              ��`                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �!           ��                            ����                            disablePagesInFolder    
      X"      �"    �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder p"      �"      �"           LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  �"      #      P#          HANDLE, getCallerWindow 0#      X#      �#    '      HANDLE, getContainerMode    h#      �#      �#    7      CHARACTER,  getContainerTarget  �#      �#      $    H      CHARACTER,  getContainerTargetEvents    �#      $      L$    [      CHARACTER,  getCurrentPage  ,$      X$      �$    t      INTEGER,    getDisabledAddModeTabs  h$      �$      �$     �      CHARACTER,  getDynamicSDOProcedure  �$      �$      %  !  �      CHARACTER,  getFilterSource �$      %      L%  "  �      HANDLE, getMultiInstanceActivated   ,%      T%      �%  #  �      LOGICAL,    getMultiInstanceSupported   p%      �%      �%  $  �      LOGICAL,    getNavigationSource �%      �%      &  %  �      CHARACTER,  getNavigationSourceEvents   �%      $&      `&  &  	      CHARACTER,  getNavigationTarget @&      l&      �&  '  #      HANDLE, getOutMessageTarget �&      �&      �&  (  7      HANDLE, getPageNTarget  �&      �&      '  )  K      CHARACTER,  getPageSource   �&       '      P'  *  Z      HANDLE, getPrimarySdoTarget 0'      X'      �'  +  h      HANDLE, getReEnableDataLinks    l'      �'      �'  ,  |      CHARACTER,  getRunDOOptions �'      �'      (  -  �      CHARACTER,  getRunMultiple  �'      (      D(  .  �      LOGICAL,    getSavedContainerMode   $(      P(      �(  /  �      CHARACTER,  getSdoForeignFields h(      �(      �(  0  �      CHARACTER,  getTopOnly  �(      �(       )  1 
 �      LOGICAL,    getUpdateSource �(      )      <)  2  �      CHARACTER,  getUpdateTarget )      H)      x)  3  �      CHARACTER,  getWaitForObject    X)      �)      �)  4        HANDLE, getWindowTitleViewer    �)      �)      �)  5        HANDLE, getStatusArea   �)       *      0*  6  +      LOGICAL,    pageNTargets    *      <*      l*  7  9      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject L*      �*      �*  8  F      LOGICAL,INPUT h HANDLE  setCallerProcedure  �*      �*       +  9  V      LOGICAL,INPUT h HANDLE  setCallerWindow  +      8+      h+  :  i      LOGICAL,INPUT h HANDLE  setContainerMode    H+      �+      �+  ;  y      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  �+      �+      ,  <  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �+      4,      d,  =  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  D,      �,      �,  >  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �,      �,       -  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource  -      @-      p-  @  �      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  P-      �-      �-  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �-      �-       .  B  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported    .      P.      �.  C        LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource l.      �.      �.  D  1      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �.      /      P/  E  E      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget 0/      t/      �/  F  _      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �/      �/      �/  G  s      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �/      0      L0  H  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   ,0      p0      �0  I  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �0      �0      �0  J  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �0      1      T1  K  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget 41      �1      �1  L  �      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �1      �1       2  M  �      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �1      $2      T2  N  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   42      x2      �2  O  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �2      �2      3  P        LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �2      <3      h3  Q 
 &      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource H3      �3      �3  R  1      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget �3      �3      4  S  A      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    �3      04      d4  T  Q      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    D4      �4      �4  U  b      LOGICAL,INPUT phViewer HANDLE   getObjectType   �4      �4      5  V  w      CHARACTER,  setStatusArea   �4      5      H5  W  �      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �5  �5      ��                  �  �  6              p�b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                                7  �6      ��                  �  �  7              �b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                8  �7      ��                  �  �  8              pa                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                9  �8      ��                  �  �  $9              a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               :  �9      ��                  �  �  (:              ��b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @:           ��                            ����                            getAllFieldHandles  (5      �:      �:  X  �      CHARACTER,  getAllFieldNames    �:      �:      ;  Y  �      CHARACTER,  getCol  �:      (;      P;  Z  �      DECIMAL,    getDefaultLayout    0;      \;      �;  [  �      CHARACTER,  getDisableOnInit    p;      �;      �;  \  �      LOGICAL,    getEnabledObjFlds   �;      �;      <  ]  �      CHARACTER,  getEnabledObjHdls   �;      <      P<  ^  �      CHARACTER,  getHeight   0<      \<      �<  _ 	       DECIMAL,    getHideOnInit   h<      �<      �<  `        LOGICAL,    getLayoutOptions    �<      �<      =  a        CHARACTER,  getLayoutVariable   �<      =      D=  b  -      CHARACTER,  getObjectEnabled    $=      P=      �=  c  ?      LOGICAL,    getObjectLayout d=      �=      �=  d  P      CHARACTER,  getRow  �=      �=      �=  e  `      DECIMAL,    getWidth    �=       >      ,>  f  g      DECIMAL,    getResizeHorizontal >      8>      l>  g  p      LOGICAL,    getResizeVertical   L>      x>      �>  h  �      LOGICAL,    setAllFieldHandles  �>      �>      �>  i  �      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �>      ?      @?  j  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout     ?      `?      �?  k  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    t?      �?      �?  l  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �?      @      <@  m  �      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    @      \@      �@  n  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout p@      �@      �@  o  �      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �@      A      <A  p  	      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   A      hA      �A  q  	      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated |A      �A      �A  r  1	      LOGICAL,    getObjectSecured    �A      B      8B  s  E	      LOGICAL,    createUiEvents  B      DB      tB  t  V	      LOGICAL,    bindServer                              C  �B      ��                  �  �  (C              ��_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               D  �C      ��                  �  �  ,D              �0b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             E  E      ��                  �  �  4E              @3b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                $F  F      ��                  �  �  <F              �3b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              0G  G      ��                  �  �  HG              Ya                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             8H   H      ��                  �  �  PH              �Ya                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             <I  $I      ��                  �  �  TI              (�a                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 lI  
         ��                            ����                            startServerObject                               lJ  TJ      ��                  �  �  �J              Dma                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                pK  XK      ��                  �  �  �K              �ma                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �K           ��                            ����                            getAppService   TB      L      8L  u  e	      CHARACTER,  getASBound  L      DL      pL  v 
 s	      LOGICAL,    getAsDivision   PL      |L      �L  w  ~	      CHARACTER,  getASHandle �L      �L      �L  x  �	      HANDLE, getASHasStarted �L      �L      M  y  �	      LOGICAL,    getASInfo   �L      (M      TM  z 	 �	      CHARACTER,  getASInitializeOnRun    4M      `M      �M  {  �	      LOGICAL,    getASUsePrompt  xM      �M      �M  |  �	      LOGICAL,    getServerFileName   �M      �M      N  }  �	      CHARACTER,  getServerOperatingMode  �M       N      XN  ~  �	      CHARACTER,  runServerProcedure  8N      dN      �N    �	      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   xN      �N      O  �  
      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �N      4O      dO  �   
      LOGICAL,INPUT pcDivision CHARACTER  setASHandle DO      �O      �O  �  .
      LOGICAL,INPUT phASHandle HANDLE setASInfo   �O      �O       P  � 	 :
      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �O       P      XP  �  D
      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  8P      |P      �P  �  Y
      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �P      �P       Q  �  h
      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �P      $Q      \Q  �  z
      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             R   R      ��                  �  �  0R              �b                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  |R             HR  
             ��   �R             pR               �� 
                 �R  
         ��                            ����                            addMessage                              �S  xS      ��                  �  �  �S               *`                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �S             �S               ��   T             �S               ��                  T           ��                            ����                            adjustTabOrder                              U  �T      ��                  �  �  $U              �;_                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  pU             <U  
             �� 
  �U             dU  
             ��                  �U           ��                            ����                            applyEntry                              �V  lV      ��                  �  �  �V              x�`                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �V           ��                            ����                            changeCursor                                �W  �W      ��                  �  �  �W              �G_                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �W           ��                            ����                            createControls                              �X  �X      ��                  �  �  �X              ,�_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �Y  �Y      ��                  �  �  �Y              ,mb                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �Z  �Z      ��                  �  �  �Z              <pb                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �[  �[      ��                  �  �  \              ,u_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �\  �\      ��                  �  �  ]              �u_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �]  �]      ��                  �  �  ^              h�_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �^  �^      ��                  �  �  _              ̡_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                               `  �_      ��                  �  �  `              ��_                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  d`             0`  
             ��   �`             X`               ��   �`             �`               ��                  �`           ��                            ����                            modifyUserLinks                             �a  �a      ��                  �  �  �a              HZ_                    O   ����    e�          O   ����    R�          O   ����    ��            ��   b             �a               ��   0b             �a               �� 
                 $b  
         ��                            ����                            removeAllLinks                               c  c      ��                  �  �  8c              �i_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                               d  d      ��                  �  �  8d               j_                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �d             Pd  
             ��   �d             xd               �� 
                 �d  
         ��                            ����                            repositionObject                                �e  �e      ��                  �  �  �e              �I`                    O   ����    e�          O   ����    R�          O   ����    ��            ��   f             �e               ��                  �e           ��                            ����                            returnFocus                             �f  �f      ��                       g              `S`                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                  g  
         ��                            ����                            showMessageProcedure                                $h  h      ��                      <h              ��`                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �h             Th               ��                  |h           ��                            ����                            toggleData                              ti  \i      ��                  	    �i              vb                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �i           ��                            ����                            viewObject                              �j  �j      ��                      �j              �%b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  <Q      k      8k  � 
 �      LOGICAL,    assignLinkProperty  k      Dk      xk  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   Xk      �k       l  �  �      CHARACTER,  getChildDataKey �k      l      <l  �        CHARACTER,  getContainerHandle  l      Hl      |l  �        HANDLE, getContainerHidden  \l      �l      �l  �  .      LOGICAL,    getContainerSource  �l      �l      �l  �  A      HANDLE, getContainerSourceEvents    �l       m      <m  �  T      CHARACTER,  getContainerType    m      Hm      |m  �  m      CHARACTER,  getDataLinksEnabled \m      �m      �m  �  ~      LOGICAL,    getDataSource   �m      �m      �m  �  �      HANDLE, getDataSourceEvents �m       n      4n  �  �      CHARACTER,  getDataSourceNames  n      @n      tn  �  �      CHARACTER,  getDataTarget   Tn      �n      �n  �  �      CHARACTER,  getDataTargetEvents �n      �n      �n  �  �      CHARACTER,  getDBAware  �n      �n      (o  � 
 �      LOGICAL,    getDesignDataObject o      4o      ho  �  �      CHARACTER,  getDynamicObject    Ho      to      �o  �        LOGICAL,    getInstanceProperties   �o      �o      �o  �        CHARACTER,  getLogicalObjectName    �o      �o      0p  �  /      CHARACTER,  getLogicalVersion   p      <p      pp  �  D      CHARACTER,  getObjectHidden Pp      |p      �p  �  V      LOGICAL,    getObjectInitialized    �p      �p      �p  �  f      LOGICAL,    getObjectName   �p      �p      ,q  �  {      CHARACTER,  getObjectPage   q      8q      hq  �  �      INTEGER,    getObjectParent Hq      tq      �q  �  �      HANDLE, getObjectVersion    �q      �q      �q  �  �      CHARACTER,  getObjectVersionNumber  �q      �q      $r  �  �      CHARACTER,  getParentDataKey    r      0r      dr  �  �      CHARACTER,  getPassThroughLinks Dr      pr      �r  �  �      CHARACTER,  getPhysicalObjectName   �r      �r      �r  �  �      CHARACTER,  getPhysicalVersion  �r      �r      (s  �  
      CHARACTER,  getPropertyDialog   s      4s      hs  �        CHARACTER,  getQueryObject  Hs      ts      �s  �  /      LOGICAL,    getRunAttribute �s      �s      �s  �  >      CHARACTER,  getSupportedLinks   �s      �s       t  �  N      CHARACTER,  getTranslatableProperties    t      ,t      ht  �  `      CHARACTER,  getUIBMode  Ht      tt      �t  � 
 z      CHARACTER,  getUserProperty �t      �t      �t  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �t      u      <u  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles u      du      �u  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    pu      �u      �u  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �u       v      Lv  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   ,v      �v      �v  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    �v      w      <w  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  w      dw      �w  �  �      CHARACTER,  setChildDataKey tw      �w      �w  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �w      �w      ,x  �  	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  x      Lx      �x  �        LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    `x      �x      �x  �  /      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �x       y      4y  �  H      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   y      \y      �y  �  \      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ly      �y      �y  �  j      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �y      z      <z  �  ~      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   z      dz      �z  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents tz      �z      �z  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �z      {      <{  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject {      \{      �{  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    p{      �{      �{  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �{      |      @|  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName     |      d|      �|  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   ||      �|      �|  �        LOGICAL,INPUT cVersion CHARACTER    setObjectName   �|      }      @}  �         LOGICAL,INPUT pcName CHARACTER  setObjectParent  }      `}      �}  �  .      LOGICAL,INPUT phParent HANDLE   setObjectVersion    p}      �}      �}  �  >      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �}      ~      @~  �  O      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks  ~      h~      �~  �  `      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   |~      �~      �~  �  t      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �~            H  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute (      l      �  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   |      �      �  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   �      �      X�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  8�      |�      ��  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      Ȁ      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ؀      8�      d�  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   D�      ��      ��  � 	        CHARACTER,INPUT pcName CHARACTER    ��    $  �  p�      ,      4   ����,                ��                      ��                  %  R                  ��b                       %  �        &  ��  �      <      4   ����<                (�                      ��                  '  Q                  ��a                       '  ��  (�    >  D�  ��      P      4   ����P                Ѓ                      ��                  J  L                  p�a                       J  T�         K                                  �     
  
       
           � ߱        T�  $  N  ��  ���                           $  P  ��  ���                       8                         � ߱        ��    V  Ȅ  D�      H      4   ����H                T�                      ��                  W  	                  $�a                       W  ؄  ��  o   Z   	   ,                                 ��  $   [  ��  ���                       �  @         �              � ߱        �  �   \  �      �  �   ]  P      �  �   _  �      0�  �   a  8      D�  �   c  �      X�  �   e         l�  �   f  �      ��  �   g  �      ��  �   j  L      ��  �   l  �      ��  �   m  <	      І  �   o  �	      �  �   p  4
      ��  �   q  p
      �  �   r  �
       �  �   s  `      4�  �   y  �      H�  �   {        \�  �   �  L      p�  �   �  �      ��  �   �  4      ��  �   �  �      ��  �   �  ,      ��  �   �  �      ԇ  �   �        �  �   �  �      ��  �   �        �  �   �  @      $�  �   �  �      8�  �   �  �      L�  �   �  d      `�  �   �  �      t�  �   �  �      ��  �   �        ��  �   �  T      ��  �   �  �      Ĉ  �   �        ؈  �   �  H      �  �   �  �       �  �   �  �      �  �   �  �      (�  �   �  8      <�  �   �  t      P�  �   �  �          �   �  �                      |�          �  Љ      ��                  B	  p	   �              �^                    O   ����    e�          O   ����    R�          O   ����    ��      \     
                �                     �                         � ߱        ��  $ V	  �  ���                           O   n	  ��  ��  (               �          �  �    �                                             ��                            ����                                �4      d�      ��     6     �                      V �  w                     x�    �	  ԋ  P�      4      4   ����4                `�                      ��                  �	  
                  �6b                       �	  �  t�  �   �	  �      ��  �   �	        ��  �   �	  �      ��  �   �	         Č  �   �	  |      ،  �   �	  �      �  �   �	  l       �  �   �	  �      �  �   �	  d      (�  �   �	  �      <�  �   �	  T      P�  �   �	  �      d�  �   �	  L          �   �	  �      P�    "
  ��  �      8      4   ����8                 �                      ��                  #
  �
                  1`                       #
  ��  4�  �   %
  �      H�  �   &
        \�  �   '
  �      p�  �   (
  �      ��  �   )
  p       ��  �   *
  �       ��  �   +
  `!      ��  �   ,
  �!      Ԏ  �   -
  H"      �  �   .
  �"      ��  �   /
  8#      �  �   0
  �#      $�  �   1
   $      8�  �   2
  �$      L�  �   3
  %      `�  �   4
  �%      t�  �   5
  &      ��  �   6
  �&      ��  �   7
  '      ��  �   8
  �'      ď  �   9
   (      ؏  �   :
  |(      �  �   ;
  �(       �  �   <
  t)      �  �   =
  �)      (�  �   >
  l*      <�  �   ?
  �*          �   @
  d+      l�    �
  l�  �      �+      4   �����+                ��                      ��                  �
  o                  X3`                       �
  |�  �  �   �
  ,,       �  �   �
  �,      4�  �   �
  $-      H�  �   �
  �-      \�  �   �
  .      p�  �   �
  �.      ��  �   �
  �.      ��  �   �
  0/      ��  �   �
  �/      ��  �   �
  �/      ԑ  �   �
  0      �  �   �
  �0      ��  �   �
  1      �  �   �
  �1      $�  �   �
  �1      8�  �   �
  h2      L�  �   �
  �2      `�  �   �
  X3      t�  �   �
  �3      ��  �   �
  4      ��  �   �
  �4      ��  �   �
  �4      Ē  �   �
  l5      ؒ  �   �
  �5      �  �   �
  �5       �  �   �
  `6      �  �   �
  �6      (�  �   �
  �6      <�  �   �
  7      P�  �   �
  P7      d�  �   �
  �7      x�  �   �
  �7      ��  �   �
  8      ��  �   �
  x8      ��  �   �
  �8      ȓ  �   �
  �8      ܓ  �   �
  ,9      �  �   �
  h9      �  �   �
  �9      �  �   �
  �9      ,�  �   �
  :      @�  �   �
  �:      T�  �   �
  ;      h�  �   �
  x;      |�  �   �
  �;      ��  �   �
  h<      ��  �   �
  �<      ��  �   �
  `=      ̔  �   �
  �=      ��  �   �
  X>      ��  �   �
  �>      �  �   �
  ?      �  �   �
  �?      0�  �   �
  �?      D�  �   �
  @      X�  �   �
  @@          �   �
  �@      ĕ  $  {  ��  ���                       A     
                    � ߱        \�    �  ��  �      (A      4   ����(A      /   �  �     ,�                          3   ����8A            L�                      3   ����XA  ��    �  x�  ��  ��  tA      4   ����tA  	              �                      ��             	     �  C                  $�_                       �  ��  �  �   �  �A      p�  $  �  D�  ���                        B     
  
       
           � ߱        ��  �   �   B      ܗ  $   �  ��  ���                       HB  @         4B              � ߱        ��  $  �  �  ���                       �B                         � ߱        C     
                �C                     �D  @        
 �D              � ߱        (�  V   �  4�  ���                        �D                     E                     XE                         � ߱        ��  $  �  Ę  ���                       F     
                �F                     �G  @        
 �G              � ߱        H�  V     T�  ���                        �G     
                lH                     �I  @        
 |I              � ߱            V   '  �  ���                        
              ��                      ��             
     E  �                  $�`                       E  t�  �I     
                DJ                     �K  @        
 TK          �K  @        
 �K          XL  @        
 L          �L  @        
 xL              � ߱            V   Z  �  ���                        adm-clone-props \�  ԛ              �     7     `                          \  J                     start-super-proc    �  @�  �           �     8                                  k                     H�    �  ̜  ܜ      DP      4   ����DP      /   �  �     �                          3   ����TP            8�                      3   ����tP  ��  $    t�  ���                       �P                         � ߱        \�    %  ��  8�  ؞  �P      4   �����P                ��                      ��                  &  *                  L}b                       &  ̝  �P                     �P                     �P                         � ߱            $  '  H�  ���                             +  ��  0�      Q      4   ����Q  $Q                         � ߱            $  ,  �  ���                       X�    3  x�  ��  ��  8Q      4   ����8Q      $  4  ��  ���                       XQ                         � ߱            �   Q  lQ      �Q     
                (R                     xS  @        
 8S              � ߱        ��  V   e  ��  ���                        ��  �   �  �S      0�      ��  Ġ      �S      4   �����S      /     �      �                          3   �����S             �                      3   �����S  �  $    \�  ���                       T                         � ߱        <T     
                �T                     V  @        
 �U              � ߱        �  V   )  ��  ���                        ��    �  4�  ��      V      4   ����V                ��                      ��                  �  �                  ��`                       �  D�      g   �  آ         ����                           ��          p�  X�      ��                  �      ��              D�`                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ̣     ܣ  <V                      3   ����$V  �     
   ��                      3   ����HV         
   ,�                      3   ����PV    ��                              ��        �                  ����                                        �              9      <�                      g                                �  g   �  �          ��	��                           إ          ��  ��      ��                  �  �  ��              �Na                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �     �  tV                      3   ����XV            4�                      3   ����|V    ��                              ��        �                  ����                                        $�              :      D�                      g                               �  g   �  �          ��	��                           �          ��  ��      ��                  �  �  ȧ              TQa                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �     �  �V                      3   �����V            <�                      3   �����V    ��                              ��        �                  ����                                        ,�              ;      L�                      g                               h�    �  $�  ��      �V      4   �����V                ��                      ��                  �  �                  Ra                       �  4�  �  /   �  ܩ     �                          3   �����V            �                      3   ����W  �  /  �  H�     X�  DW                      3   ����$W  ��     
   x�                      3   ����LW  ��        ��                      3   ����TW  �        ت                      3   ����hW            �                      3   �����W  @�    �  4�  D�      �W      4   �����W      /  �  p�     ��  8X                      3   ����X  ��     
   ��                      3   ����@X  �        Ы                      3   ����HX  �         �                      3   ����\X            0�                      3   �����X        �  \�  l�      �X      4   �����X      /  �  ��     ��  �X                      3   �����X  ج     
   Ȭ                      3   �����X  �        ��                      3   ����Y  8�        (�                      3   ����Y            X�                      3   ����4Y  (�    �  ��   �      XY      4   ����XY                �                      ��                  �  �                  ,�E                       �  ��      g   �  (�         ��̯        hY                  �          ��  ��      ��                  �      خ              l�a                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �     ,�  �Y                      3   ����tY  \�     
   L�                      3   �����Y         
   |�                      3   �����Y    ��                            ����                                        <�              <      ��                      g                               ��     �  �Y                                     �Y     
                8Z                     �[  @        
 H[              � ߱        P�  V   a  \�  ���                        �[     
                \                     h]  @        
 (]              � ߱        |�  V   �  �  ���                         �    �  ��  ��      |]      4   ����|]      $   �  Ա  ���                       �]  @         �]              � ߱        Գ  g   �  �         ��x�        �]  ��x�        �]                  ��          Ĳ  ��      ��                  �  �  ܲ              $�_                    O   ����    e�          O   ����    R�          O   ����    ��            �  �   �      ^      4   ����^      O  �  ������  ^    ��                            ����                                        @�              =      8�                      g                               ��  g   �  �         �6$�         0^                  ��          ��  l�      ��                  �  �  ��              ��_                    O   ����    e�          O   ����    R�          O   ����    ��      ̴    �  <^  }          O  �  ������  P^    ��                            ����                                         �              >      �                      g                               4�  g   �  ��         �"ض                           `�          0�  �      ��                  �  �  H�              8�_                    O   ����    e�          O   ����    R�          O   ����    ��            �  d^  }        ��                              ��        �                  ����                                        ��              ?      x�                      g                               L�  g   �  L�         �"�                           ��          �  ̷      ���               �    ��              ��F                    O   ����    e�          O   ����    R�          O   ����    ��                                                                                                   � ߱        ��  $   �  �   �                       ��  /   �  �                                 3   ����x^  ��  A  �       L�   ��                                                                 ��  ��                                   @            h�   x�    ��    �  Ĺ  @�      �^      4   �����^                P�                      ��                  �  �                  0IE                       �  Թ  ��  	  �  ��                                        3   �����^      O  �  ������  �^  ��  /     غ     �                          3   �����^  p�        �  �                  3   �����^      $     D�  ���                                                   � ߱                  ��  ��                  3   �����^      $     ̻  ���                                                   � ߱        <�      �  $�      �^      4   �����^      O    ������  _  �  /     h�     x�                          3   ����,_  ��     
   ��                      3   ����D_  ؼ        ȼ                      3   ����X_            ��                      3   ����d_      	    <�                                        3   ����p_                ��                                           ��                              ��        �                  ����                                  T�          `�  L�         @     ��                      g   ��                                "  h�  �      |_      4   ����|_                X�                      ��                  "  N                  d�E                       "  x�  �_  @                     �_  @         �_          �_  @         �_              � ߱        ��  $   #  ��  ���                       ��  g   )  ��         �n$�      }                      d�          4�  �      ��                  *  .  L�              ةE                    O   ����    e�          O   ����    R�          O   ����    ��      ��  /  +  ��                                 3   �����_        ,  ��  ��      `      4   ����`      O  -  ������  <`    ��                            ����                                        ��              A      ��                      g                               T�  g   3  ��         �!��         P`                  ��          0�  �      ��                  3  5  H�              ��E                    O   ����    e�          O   ����    R�          O   ����    ��      \`  @                         � ߱            $  4  `�  ���                         ��                            ����                                        ��              B      ��                      g                               ��  /   8  ��                                 3   ����d`        ?  ��  (�      �`      4   �����`                ��                      ��                  ?  L                  (�E                       ?  ��                ��          ��  ��      ��                 C  J                  �G                       C  8�      O   C    ��          O   C    ��       �  /   G  �                                 3   �����`        H  <�  L�      �`      4   �����`      k   I  h�              }       n        �   adm-create-objects  �  ��                      C      �                               j                     Carga-temporal  ��  ��          0         D     t                          p  H                      disable_UI   �  \�                      E      <                              W   
                   enable_UI   h�  ��                      F                                     b   	                   exitObject  ��  ,�                      G      �                               l   
                   initializeObject    8�  ��                      H                                    ~                      PrecioListaMinorista    ��  �  �       t         I     �                          �  :!                      � ��   �   �Todas�/�� ���  �       ��  8   ����"   ��  8   ����"   ��  "  ��  8   ����!    �  8   ����!   �  !  �  8   ����    (�  8   ����    ��     8�  8   ����   H�  8   ����   X�  8   ����   h�  8   ����   x�  8   ����   ��  8   ����   ��  8   ����   ��  8   ����   ��        8   ����       8   ����             ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  �  (�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL   �  l�  x�      returnFocus ,INPUT hTarget HANDLE   \�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  P�  `�      removeAllLinks  ,   @�  t�  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE d�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  h�  t�      hideObject  ,   X�  ��  ��      editInstanceProperties  ,   x�  ��  ��      displayLinks    ,   ��  ��  ��      createControls  ,   ��  ��  �      changeCursor    ,INPUT pcCursor CHARACTER   ��  8�  D�      applyEntry  ,INPUT pcField CHARACTER    (�  p�  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER `�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  <�  D�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ,�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  ��      startServerObject   ,   ��  ��  �      runServerObject ,INPUT phAppService HANDLE  ��  4�  H�      restartServerObject ,   $�  \�  t�      initializeServerObject  ,   L�  ��  ��      disconnectObject    ,   x�  ��  ��      destroyServerObject ,   ��  ��  ��      bindServer  ,   ��  ��  �      processAction   ,INPUT pcAction CHARACTER   ��  4�  D�      enableObject    ,   $�  X�  h�      disableObject   ,   H�  |�  ��      applyLayout ,   l�  ��  ��      viewPage    ,INPUT piPageNum INTEGER    ��  ��  ��      viewObject  ,   ��  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  (�  4�      selectPage  ,INPUT piPageNum INTEGER    �  `�  t�      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER P�  ��  ��      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  �  �      notifyPage  ,INPUT pcProc CHARACTER ��  8�  D�      initPages   ,INPUT pcPageList CHARACTER (�  p�  ��      initializeVisualContainer   ,   `�  ��  ��      hidePage    ,INPUT piPageNum INTEGER    ��  ��  ��      destroyObject   ,   ��  ��  �      deletePage  ,INPUT piPageNum INTEGER    ��  4�  D�      createObjects   ,   $�  X�  h�      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE H�  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  �  $�      changePage  ,   �  8�  L�      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 `%     adecomm/as-utils.w 
"   
   �    }        �
"     
   %                  �     }        �G� �   �G%              � �  3   %         %       P       %        %       %        %       %               %               %               %              %              %              %               %              
�        
"   
 `
�    
"   
 `
"   
 �    �        �     �            
"   
   �        D         �     }        �%              
"   
 `
"   
 �    �        �     �        �    
"   
   �        �         �     }        �%              � 
" 
   
 �%              � �  �         X      $              
�    � 
   �     
"   
 b                      
�            �    �
" 
   
 �
�H T   %              �     }        �GG %              � 
"  
 
   P �L 
�H T   %              �     }        �GG %              
"  	 
   �        �    7%               
"  	 
 s�           �    1�   
 s� '   �%               o%   o           � ,    s
"  	 
 s�           D    1� -   s� '   �%               o%   o           � ;   s
"  	 
 s�           �    1� B  
 s� '   �%               o%   o           � M   s
"  	 
 s�           ,    1� Y   s� '   �%               o%   o           � g   s
"  	 
 s�           �    1� n   s� '   �%               o%   o           � }   s
"  	 
 s�               1� �   s� �   �%               o%   o           %               
"  	 
 ��          �    1� �   �� �     
"  	 
 s�           �    1� �   s� '   �%               o%   o           � �  e s
"  	 
 s�           @    1� 8   s� '   �%               o%   o           � G  [ s
"  	 
 s�           �    1� �   s� �   �%               o%   o           %               
"  	 
 s�           0	    1� �   s� �   �%               o%   o           %               
"  	 
 s�           �	    1� �   s� �   �%               o%   o           %              
"  	 
 ��          (
    1� �   �� �     
"  	 
 s�           d
    1� �  
 s� �   �%               o%   o           %               
"  	 
 s�           �
    1� �   s� '   �%               o%   o           � ,    s
"  	 
 ��          T    1� �   �� �     
"  	 
 s�           �    1�    s� '   �%               o%   o           �   t s
"  	 
 ��              1� �  
 �� �     
"  	 
 s�           @    1� �   s� '   �%               o%   o           � �  � s
"  	 
 s�           �    1� 8   s� '   �%               o%   o           � ,    s
"  	 
 s�           (    1� O  
 s� Z   �%               o%   o           %               
"  	 
 _�           �    1� ^   _� �   �%               o%   o           %               
"  	 
 _�                1� f   _� '   �%               o%   o           � ,    _
"  	 
 _�           �    1� w   _� '   �%               o%   o           o%   o           
"  	 
 b�               1� �  
 b� '   �%               o%   o           � ,    `
"  	 
 _�           �    1� �   _� �  	 �%               o%   o           � �  / b
"  	 
 ��          �    1� �   �� �  	   
"  	 
 `�           4    1� �   `� �  	 �o%   o           o%   o           � ,    `
"  	 
 ��          �    1�    �� �  	   
"  	 
 _�           �    1�    _� �  	 �o%   o           o%   o           � ,    _
"  	 
 ��          X    1� !   �� �     
"  	 
 ��          �    1� /   �� �  	   
"  	 
 ��          �    1� <   �� �  	   
"  	 
 ��              1� I   �� �  	   
"  	 
 _�           H    1� W   _� �   �o%   o           o%   o           %              
"  	 
 ��          �    1� h   �� �  	   
"  	 
 ��               1� v  
 �� �     
"  	 
 ��          <    1� �   �� �  	   
"  	 
 ��          x    1� �   �� �  	   
"  	 
 ��          �    1� �   �� �  	   
"  	 
 ��          �    1� �   �� �  	   
"  	 
 ��          ,    1� �  	 �� �  	   
"  	 
 ��          h    1� �   �� �  	   
"  	 
 ��          �    1� �   �� �  	   
"  	 
 _�           �    1�    _� '   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 _
"   
   
"   
 �(�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�       �    ��      p�               �L
�    %              � 8      �    � $         �           
�    � 9     
"   
 �� @  , 
�       �    �� B  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"  	 
 a�           �    1� <  
 a� '   �%               o%   o           � ,    a
"  	 
 a�           �    1� G  
 a� '   �%               o%   o           o%   o           
"  	 
 `�           x    1� R   `� �   �%               o%   o           o%   o           
"  	 
 _�           �    1� [   _� �   �%               o%   o           %               
"  	 
 _�           p    1� j   _� �   �%               o%   o           %               
"  	 
 `�           �    1� w   `� '   �%               o%   o           � ,    _
"  	 
 _�           `    1� ~   _� �   �%               o%   o           %              
"  	 
 _�           �    1� �   _� �   �%               o%   o           o%   o           
"  	 
 b�           X    1� �   b� '   �%               o%   o           o%   o           
"  	 
 `�           �    1� �  	 `� '   �%               o%   o           � ,    `
"  	 
 `�           H    1� �   `� '   �%               o%   o           o%   o           
"  	 
 ^�           �    1� �   ^� '   �%               o%   o           o%   o           
"  	 
 _�           @    1� �   _� �   �%               o%   o           %               
"  	 
 _�           �    1� �   _� �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"  	 
 _�           �    1� �   _� �  	 �%               o%   o           � ,    _
"  	 
 b�                1�     b� �  	 �%               o%   o           � ,    _
"  	 
 a�           t    1�    a� �   �%               o%   o           %               
"  	 
 `�           �    1�    `� �  	 �%               o%   o           � ,    a
"  	 
 ^�           d     1� +   ^� �  	 �%               o%   o           � ,    `
"  	 
 _�           �     1� 9   _� �   �%               o%   o           %               
"  	 
 _�           T!    1� G   _� �  	 �%               o%   o           � ,    _
"  	 
 a�           �!    1� V   a� �  	 �%               o%   o           � ,    _
"  	 
 _�           <"    1� e   _� �  	 �%               o%   o           � ,    a
"  	 
 _�           �"    1� s   _� �  	 �%               o%   o           o%   o           
"  	 
 a�           ,#    1� �   a� �  	 �%               o%   o           � ,    b
"  	 
 `�           �#    1� �   `� �  	 �%               o%   o           � ,    a
"  	 
 ^�           $    1� �  	 ^� �   �%               o%   o           %               
"  	 
 _�           �$    1� �   _� �   �%               o%   o           %               
"  	 
 _�           %    1� �   _� �   �%               o%   o           o%   o           
"  	 
 _�           �%    1� �   _� �   �%               o%   o           o%   o           
"  	 
 _�           &    1� �   _� �   �%               o%   o           %               
"  	 
 b�           �&    1� �   b� �   �%               o%   o           %               
"  	 
 a�           �&    1� �   a� �   �%               o%   o           %               
"  	 
 `�           x'    1�    `�    �%               o%   o           %       
       
"  	 
 `�           �'    1�    `�    �%               o%   o           o%   o           
"  	 
 _�           p(    1� &   _�    �%               o%   o           %              
"  	 
 _�           �(    1� 2   _�    �%               o%   o           o%   o           
"  	 
 `�           h)    1� >   `�    �%               o%   o           %              
"  	 
 `�           �)    1� K   `�    �%               o%   o           o%   o           
"  	 
 b�           `*    1� X   b�    �%               o%   o           %              
"  	 
 b�           �*    1� `   b�    �%               o%   o           o%   o           
"  	 
 `�           X+    1� h   `� �  	 �%               o%   o           � ,    aP �L 
�H T   %              �     }        �GG %              
"  	 
 ^�            ,    1� z   ^� Z   �%               o%   o           %               
"  	 
 ^�           �,    1� �   ^� Z   �%               o%   o           o%   o           
"  	 
 _�           -    1� �   _� '   �%               o%   o           � ,    _
"  	 
 `�           �-    1� �   `� '   �%               o%   o           � �  - _
"  	 
 a�            .    1� �   a� '   �%               o%   o           � ,    `
"  	 
 b�           t.    1� �   b� '   �%               o%   o           �    a
"  	 
 ��          �.    1� 8   �� �     
"  	 
 `�           $/    1� I   `� '   �%               o%   o           � ,    _
"  	 
 ��          �/    1� U  
 �� �     
"  	 
 ��          �/    1� `   �� �     
"  	 
 _�           0    1� m   _� �  	 �%               o%   o           � ,    _
"  	 
 `�           �0    1� z   `� '   �%               o%   o           � ,    _
"  	 
 `�           �0    1� �   `� �   �%               o%   o           o%   o           
"  	 
 b�           t1    1� �   b� '   �%               o%   o           � �  ! _
"  	 
 a�           �1    1� �   a� '   �%               o%   o           � ,    b
"  	 
 `�           \2    1� �   `� '   �%               o%   o           � �   a
"  	 
 `�           �2    1� �  	 `� Z   �%               o%   o           o%   o           
"  	 
 _�           L3    1�    _� �   �%               o%   o           %               
"  	 
 ��          �3    1�    �� �     
"  	 
 `�           4    1�    `� '   �%               o%   o           � 0   a
"  	 
 _�           x4    1� ?   _� �  	 �%               o%   o           � ,    `
"  	 
 b�           �4    1� L   b� �  	 �%               o%   o           � ,    _
"  	 
 ��          `5    1� \   �� �     
"  	 
 ��          �5    1� n   �� �  	   
"  	 
 `�           �5    1� �   `� �   �o%   o           o%   o           %               
"  	 
 ��          T6    1� �   �� �     
"  	 
 ��          �6    1� �   �� �  	   
"  	 
 ��          �6    1� �   �� �  	   
"  	 
 ��          7    1� �   �� �  	   
"  	 
 ��          D7    1� �   �� �  	   
"  	 
 ��          �7    1� �   �� �  	   
"  	 
 ��          �7    1�    �� �     
"  	 
 b�           �7    1�    b� '   �%               o%   o           � +  4 ^
"  	 
 ��          l8    1� `   �� �     
"  	 
 ��          �8    1� m   �� �     
"  	 
 ��          �8    1� }   �� �     
"  	 
 ��           9    1� �   �� �  	   
"  	 
 ��          \9    1� �   �� �  	   
"  	 
 ��          �9    1� �   �� �  	   
"  	 
 ��          �9    1� �   �� �     
"  	 
 _�           :    1� �   _� �  	 �%               o%   o           � ,    `
"  	 
 a�           �:    1� �   a� �  	 �%               o%   o           � ,    _
"  	 
 ^�           �:    1� �   ^� �  	 �%               o%   o           � ,    a
"  	 
 b�           l;    1� �   b� �  	 �%               o%   o           � ,    ^
"  	 
 _�           �;    1�    _� �   �%               o%   o           %               
"  	 
 _�           \<    1� !   _� �   �%               o%   o           o%   o           
"  	 
 _�           �<    1� 3   _� �   �%               o%   o           %               
"  	 
 `�           T=    1� C   `� �   �%               o%   o           %               
"  	 
 `�           �=    1� O   `� �   �%               o%   o           o%   o           
"  	 
 a�           L>    1� j   a� �   �%               o%   o           %               
"  	 
 ��          �>    1� x   �� �  	   
"  	 
 `�           ?    1� �   `� �   �%               o%   o           %              
"  	 
 ��          �?    1� �   �� �  	   
"  	 
 ��          �?    1� �   �� �  	   
"  	 
 ��          �?    1� �  
 �� �  	   
"  	 
 `�           4@    1� �   `� �  	 �%               o%   o           �    _
"  	 
 a�           �@    1� �   a� �  	 �%               o%   o           � ,    `
"   
    "    �%     start-super-proc ��%     adm2/smart.p �P �L 
�H T   %              �     }        �GG %              
"  	 
   �       �A    6�      
"  	 
   
�        �A    8
"  
 
   �        B    ��     }        �G 4              
"  
 
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        \C    ��    � P   �        hC    �@    
� @  , 
�       tC    ��    �p�               �L
�    %              � 8      �C    � $         �           
�    � 9   �
"   
 �p� @  , 
�       �D    �� �   �p�               �L"    , �   �    _�    ��     }        �A      |    "      �    a%              (<   \ (    |    �     }        �A�    �A"    _    "    �"    _  < "    �"    _(    |    �     }        �A�    �A"    _
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        dF    ��    � P   �        pF    �@    
� @  , 
�       |F    ��    �p�               �L
�    %              � 8      �F    � $         �           
�    � 9   �
"   
 �p� @  , 
�       �G    ��   
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        <H    ��    � P   �        HH    �@    
� @  , 
�       TH    ��    �p�               �L
�    %              � 8      `H    � $         �           
�    � 9   �
"   
 �p� @  , 
�       pI    �� �   �p�               �L
"   
 , 
�H T   %              �     }        �GG %              
"   
   
"   
 `
"   
   
"   
   (�  L ( l       �        J    ��    � P   �         J    �@    
� @  , 
�       ,J    ��      p�               �L
�    %              � 8      8J    � $         �           
�    � 9     
"   
 �p� @  , 
�       HK    �� B  
 �p�               �L%     SmartWindow 
"   
   p� @  , 
�       �K    �� Y     p�               �L%      WINDOW  
"   
  p� @  , 
�       L    ��     p�               �L%               
"   
  p� @  , 
�       lL    �� �    p�               �L(        � ,      � ,      � ,      �     }        �A
�H T   %              �     }        �GG %              
"   
 a (   � 
"   
 �    �        LM    ��    �
"   
   � 8      �M    � $         �           
�    � 9   �
"   
   �        �M    �
"   
   �       N    /
"   
   
"   
   �       <N    6�      
"   
   
�        hN    8
"   
   �        �N    �
"   
   �       �N    �
"   
   p�    � 9   b
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        lO    �A"    �A
"   
   
�        �O    �@ � 
"   
 a"      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p �b�    � �     
�    �     }        �%               %      Server  - �     }        �    "    _� ,    �%                   "    _� ,    �%      NONE    p�,  8         $     "    _        � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �Q    ��    � P   �        R    �@    
� @  , 
�       R    ��    �p�               �L
�    %              � 8      R    � $         �           
�    � 9   �
"   
 �p� @  , 
�       ,S    �� �   �p�               �L"    , p�,  8         $     "    _        � �   �
�     "    �%     start-super-proc ��%     adm2/visual.p ��   � 
     �      �   T   
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �T    ��    � P   �        �T    �@    
� @  , 
�       �T    ��    �p�               �L
�    %              � 8      �T    � $         �           
�    � 9   �
"   
 �p� @  , 
�       �U    �� G   �p�               �L"    , � 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP �%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc ��%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents a%      initializeDataObjects a0 0   A    �    � �   a
�    � �   �A    �    � �     
�    � �   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents a%     buildDataRequest ent0 A    �    � �   �
�    � �   _%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
"    
 �
"   
 �%     contextHelp 
"    
   
�    
�    %               
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 `(�  L ( l       �        Z    ��    � P   �        Z    �@    
� @  , 
�        Z    ��    �p�               �L
�    %              � 8      ,Z    � $         �    �     
�    � 9   �
"   
 �p� @  , 
�       <[    �� \   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        �[    ��    � P   �        �[    �@    
� @  , 
�        \    ��    �p�               �L
�    %              � 8      \    � $         �    �     
�    � 9   �
"   
 �p� @  , 
�       ]    ��    �p�               �L%              (        �     }        �G� �   �G� 
"   
 �
"   
   �        �]    �%              
"   
 �
"   
 ��     }        �%               
"   
 �%      CLOSE   %               %      CLOSE   %     Carga-temporal   *    �      %               %     lib/tt-file-to-text "      "          "    ^� 9    �%               %     lib/tt-file 
�             �G"      "      � C     � 
"   
 �
"   
 ^
"   
 ��        �_    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � `  	   %               
"   
 �
�    %     createObjects    �     }        �%     initializeObject �� �     }        �� M   �%              � �     "           "      &    T   &    "      &    &    &     ,   %                  &        "      &    �      �%               �       � 
      "       "      "      "      &    &    &    &    &    &    &    &    �    �    �    d    0 ,       %              %                  S    "      &    &    %              %                  "      &    %              �            B p     \     H     4          �           "  1    �       �       "      �       "      "      "  1    "      "      &    &    &    &    &    &    &    &    L    0        %              %              %              %              "      "      &    &    &    &        %              %              %      PrecioListaMinorista �"  1  �"    �"    �"      "      "      %              "  /    "          "      "          "      "      8 (   $          "    �"    a"    �    "      %       d       "  1    "      "      "      "      "      "      "      "      "      "      "      "      (        �     }        �G� �   �G� 
"   
 �
"   
   �     }        �
�    
"   
 �"    `"    �"      "      "      
"   
 `
"   
   %      CLOSE   %               � w    �"     �&    &        %              8    "      &    � 4                      "      � �     "               +   +  %              +  %      SUPER   "     �"    �&    &    &    &        %              %              "    �%              "      "      &    &    &    &        %              %               *     %               "       "  #  �"    �&    &    &    &        %              %               * !   %                   " !     "      "           "    `%                  "       %              "           "       "          "    a%                  "       %              "           "       "      %              "    �"    �"    �&    &    &    &    &    &    0        %              %              %               * "   %               " "     "  #  �"    �&    &    &    &        %              %               * !   %                   " !     "      " "         "    _%                  " "     %              " "         " "     "          "    a%                  " "     %              " "         " "     "                      �           �   l       ��                 R  v  �               h�`                    O   ����    e�          O   ����    R�          O   ����    ��        $  a  �   ���                        M     
                    � ߱              b  (  �      XM      4   ����XM                �                      ��                  c  u                  ``                       c  8  �  �  d  �M            f  �  `      �M      4   �����M                p                      ��                  g  t                  y`                       g  �  �  o   h      ,                                 �  �   i  N      �  �   j  HN      $  $  k  �  ���                       tN     
                    � ߱        8  �   l  �N      L  �   m  �N      `  �   p  �N          $   s  �  ���                       O  @         �N              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               Tz`                    O   ����    e�          O   ����    R�          O   ����    ��      Z                      �          �  $  �    ���                       XO     
                    � ߱                  �  �                      ��                   �  �                  X?`                     �  4      4   ����xO      $  �  �  ���                       �O     
                    � ߱        �    �  4  D      �O      4   �����O      /  �  p                               3   �����O  �  �   �  �O          O   �  ��  ��  0P                               , �                          
                               �      ��                            ����                                                        �   l       ��                  Y  `  �               h	G                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                 f  �  �               �,G                    O   ����    e�          O   ����    R�          O   ����    ��      0  �   z           @      l          �  �      ��                  {  �  �              ��G                       {  �       l  �       ��                            7   ����          ��               Xa    �                              6   {        \   ��         0  Xa    �                                                                    �`   �`   �`   �`   a   $a                   �  �           Ha           Pa                      x   �        O   ����  e�          O   ����  R�          O   ����  ��            |      `          0        ��                  }  �  H              �G                       }         �  �       ��                            7   ����
          ��               Hb    �            H                  6   }  
      �   ��         l  Hb    �            H                                                        �a   �a   �a   �a   �a   �a   �a   �a                     �           b  b  (b  8b           b   b  0b  @b             	 
       	 �  
 �        O   ����  e�          O   ����  R�          O   ����  ��      $  $   �  �  ���                       8c  @         $c              � ߱              4      h	  4      8	   	      ��                  �  �  P	              �*F                       �  �  �  `  �       ��                            7   ����          ��               ,d    �                               6   �        D   ��         $  ,d    �                                                                     �c   �c   �c   �c                   �  �           �c  �c  d  d           �c  d  d  $d                      `   |        �  4       ��$                           A   ����          ��               �d    �            �                  6   �        �   ��         �  �d    �            �                          *                              �d   �d                   	   	           �d  �d           �d  �d         �            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �  /   �  �	     �	                          3   �����d  �	        �	                      3   ���� e  
        �	                      3   ����,e  �
        $
  4
                  3   ����8e      $   �  `
  ���                                                   � ߱                �
  �
                  3   ����De      $   �  �
  ���                                                   � ߱        D        4                      3   ����Pe  t        d                      3   ����\e  �        �                      3   ����he  �        �                      3   ����|e            �                    3   �����e      $   �  0  ���                                                   � ߱        �e                     �e                         � ߱        �  $  �  \  ���                             �  �  h      �e      4   �����e                x                      ��                  �  �                  �!G                       �  �    9   �     <f                     Hf                     Tf                     `f                     lf                     xf                     �f                     �f       	       	       �f       
       
       �f                     �f                     �f                     �f                         � ߱            $  �  �  ���                                     d                                                   ��                             ��                             ��                              ��        �                   ��                            ����                                =   �                     �           �   l       ��                  �  �  �               4UE                    O   ����    e�          O   ����    R�          O   ����    ��           �  �   �       �f      4   �����f      n   �     �          g        �    ,      $g      4   ����$g      �   �  8g    ��                            ����                                                      �   l       ��                  �  �  �               $VE                    O   ����    e�          O   ����    R�          O   ����    ��      Lg  �           Xg  �          dg  �          pg  �          |g  �              � ߱        �  Z   �  �    �        @g                  �               �              �              �              �              �              � ߱        �  h   �  @   �        �g                  
   �  �� �             �g    ��                              ��        �                  ����                                            �           �   l       ��                  �  �  �               X�E                    O   ����    e�          O   ����    R�          O   ����    ��      �     �  �g  }          O   �  ��  ��  �g    ��                            ����                                                       �   l       ��                      �               ��E                    O   ����    e�          O   ����    R�          O   ����    ��                    �                      ��                                      OF                �       �         �      T          $        ��                  	    <              |OF                �     	  0      �  (       ��                            7   ����          ��               �g    �            x                  6   	        �   ��         �  �g    �            x                                                        �g   �g                   �  �           �g           �g                      �   �        O   ����  e�          O   ����  R�          O   ����  ��          �     (h      ph                     �h                         � ߱            $    h  ���                           /                                     3   �����h    ��                              ��        �                   ��                            ����                                                      �   l       ���                 n  �               ~G                    O   ����    e�          O   ����    R�          O   ����    ��      �        �              �          �                     �          �        8                      �       `             ,         �        �             T         �        �             |         �        �             �         �   	                   �         �   
                    �         �  A   0       �   ��         l  �h                                        �h   �h                   �  �           �h  �h           �h  �h         �            �   �        p   4   i         l  �	  |     ,i                �                      ��                  5  P                  �G                       5    T  A  6         �   ��         �  xi                                         @i   Li                   @  4           Xi  hi           `i  pi         �                    �    7  p  �      �i      4   �����i      O   7  ��  ��  �i  �i                         � ߱        �  $  8  �  ���                       �  A  <       ! T   ��        	 @  j                                        �i   �i                   �  �           �i  �i           �i  j         �            p   �    �    ?  �  �      <j      4   ����<j      O   ?  ��  ��  Hj  T  $  @  (  ���                       \j                         � ߱        �  $  C  �  ���                       |j                         � ߱        0    F  �  D      �j      4   �����j                T                      ��                  F  J                  �
G                       F  �        G  p  �    �j      4   �����j  �j       
       
           � ߱            $  H  �  ���                       �j       
       
           � ߱            $  I  �  ���                             K  L  �      k      4   ����k                �                      ��                  K  O                  `G                       K  \        L  �  0	  �	  ,k      4   ����,k  Tk       
       
           � ߱            $  M  	  ���                       `k       
       
           � ߱            $  N  \	  ���                           0
     �k                @
                      ��                  Q  k                  G                       Q  �	    A  R       " �
   ��        
 �
  �k                                         �k   �k   �k                    �
           �k  �k  �k           �k  �k  �k         �            �
   �
    �    S  0  @      4l      4   ����4l      O   S  ��  ��  @l  Tl                         � ߱        �  $  T  X  ���                       x  A  W       !    ��            �l                                        `l   ll                   d  X           xl  �l           �l  �l         �            0   D    �    Z  �  �      �l      4   �����l      O   Z  ��  ��  �l    $  [  �  ���                       �l                         � ߱        l  $  ^  @  ���                       m                         � ߱        �    a  �        m      4   ����m                                      ��                  a  e                  ��F                       a  �        b  0  l  �  <m      4   ����<m  dm       
       
           � ߱            $  c  @  ���                       pm       
       
           � ߱            $  d  �  ���                             f    �      �m      4   �����m                �                      ��                  f  j                  l�F                       f          g  �  �  H  �m      4   �����m  �m       
       
           � ߱            $  h  �  ���                       �m       
       
           � ߱            $  i    ���                                     �                                                   ��                            ����                               "    !                D   d d     �   ��@mA  � �                                               �                                                                         d     D                                                                 P   4� d                                                           W!  G     p  4� �l                                                         :     �                      M   P   4;hd                                                           `!  G   
 X  4;xd                                                        b     �  
    P   4� d                                                           i!  G   
 X  4�xd                                                        s     �  
    P   4�d                                                           r!  G   
 X  4xd                                                        S     �     
 X   ~pd                                                        �     �      `  �~                                                          (        $                  \  ���                                           
       �!      �        @      `  �                                                                  $                  \  ���             d                   
                 �!      �        H       D                                                                    TXS appSrvUtils ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST s-codcia s-task-no Detalle coddiv coddoc nrodoc fchdoc codmat desmat prevta undvta prebas undbas univta unibas undstk wWin BtnDone img/exit.ico BUTTON-1 img/print (2).ico COMBO-BOX-Division Todas FILL-IN-Factor FILL-IN-FchDoc-1 FILL-IN-FchDoc-2 FILL-IN-Mensaje fMain X(256) 99/99/9999 >>9.99 GUI DIFERENCIAS DE PRECIOS COMPROBANTES MANUALES UTILEX DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   COMBO-BOX-Division FILL-IN-FchDoc-1 FILL-IN-FchDoc-2 FILL-IN-Factor BUTTON-1 BtnDone CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE No hay registros a imprimir pOptions  pArchivo Proceso terminado iStartPage ADM-ERROR ADM-CREATE-OBJECTS x-PreUni x-PreLis f-CanPed s-UndVta f-Factor f-PreBas f-PreVta f-Dsctos y-Dsctos z-Dsctos x-TipDto GN-DIVI DIVISIONES  -  CcbCDocu FAC,BOL A M PROCESANDO  x(5)   CcbDDocu Almmmatg Cat�logo de Materiales CARGA-TEMPORAL DISABLE_UI ENABLE_UI EXITOBJECT UTILEX INITIALIZEOBJECT S-CODDIV S-CODMON S-UNDVTA S-CODMAT X-CANPED x-NroDec s-FlgSit F-PREBAS s-TpoCmb VtaListaMinGn Almtconv Tabla de Conversiones de Unid. VtaListaMin Lista precios minorista PRECIOLISTAMINORISTA default Divisi�n Desde el Hasta el Diferencia en % Button 1 &Done IDX01 llave05 llave04 Matg01 Indice01 conv01 idx01   �$  8  �+      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   V	  n	  p	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props a  b  c  d  f  g  h  i  j  k  l  m  p  s  t  u  v              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  T	  �	     =                                   �  �  �	  �	     >                                   �  �  �  �	  ,
     ?                                   �  �  T
        H
     pOptions              h
     pArchivo    �	  �
     @   4
                              �  �  �  �  �  �  �            t
       A                                   +  ,  -  .  �
  D     B                                   4  5    �     C               |                  adm-create-objects  `  �        �     x-PreUni    �        �     x-PreLis    �        �     f-CanPed                 s-UndVta    4        (     f-Factor    T        H     f-PreBas    t        h     f-PreVta    �     	   �     f-Dsctos    �     
   �     y-Dsctos    �        �     z-Dsctos              �     x-TipDto    L  4     D   �          $                  Carga-temporal  z  {  }  �  �  �  �  �  �  �  �  �  �  �  �  �  �     E               �                  disable_UI  �  �  �  �  p  �     F               �                  enable_UI   �  �  �  �  �  D     G               8                  exitObject  �  �  �    �     H               �                  initializeObject      	                        �     s-TpoCmb    �        �        S-CODDIV                    S-CODMON    @        4        S-UNDVTA    d        X        f-Factor    �        |        S-CODMAT    �        �        X-CANPED    �        �        x-NroDec    �     	   �        s-FlgSit           
           F-PREBAS    P  `  *   I   �  �      H                  PrecioListaMinorista    0  4  5  6  7  8  <  ?  @  C  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  W  Z  [  ^  a  b  c  d  e  f  g  h  i  j  k  l  n    �       T      �                          H  P     Detalle �         �         �                                             $         ,         4         <         D         L         coddiv  coddoc  nrodoc  fchdoc  codmat  desmat  prevta  undvta  prebas  undbas  univta  unibas  undstk  t          h  
   appSrvUtils �        �     s-codcia    �       �     s-task-no   �       �  
   wWin    �       �     COMBO-BOX-Division              FILL-IN-Factor  D       0     FILL-IN-FchDoc-1    l       X     FILL-IN-FchDoc-2    �       �     FILL-IN-Mensaje �        �  
   gshAstraAppserver   �        �  
   gshSessionManager           �  
   gshRIManager    ,          
   gshSecurityManager  T  	 	     @  
   gshProfileManager   �  
 
     h  
   gshRepositoryManager    �        �  
   gshTranslationManager   �        �  
   gshWebManager   �        �     gscSessionId                 gsdSessionObj   <        ,  
   gshFinManager   `        P  
   gshGenManager   �        t  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj  �        �     gsdRenderTypeObj                 gsdSessionScopeObj  4       ,  
   ghProp  T    	   H  
   ghADMProps  x    
   h  
   ghADMPropsBuf   �       �     glADMLoadFromRepos  �       �     glADMOk �       �  
   ghContainer �       �     cObjectName             iStart  8       ,     cAppService X       L     cASDivision �       l     cServerOperatingMode    �       �     cFields          �     iStartPage  �    L  �  Detalle �       �  GN-DIVI           CcbCDocu    (         CcbDDocu    D       8  Almmmatg    d        T  VtaListaMinGn   �   !    t  Almtconv         "    �  VtaListaMin          7   ]  ^  o  �  �  �  �  �  �  �  $  %  &  '  >  J  K  L  N  P  Q  R  V  W  Z  [  \  ]  _  a  c  e  f  g  j  l  m  o  p  q  r  s  y  {  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  "
  #
  %
  &
  '
  (
  )
  *
  +
  ,
  -
  .
  /
  0
  1
  2
  3
  4
  5
  6
  7
  8
  9
  :
  ;
  <
  =
  >
  ?
  @
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
  o  {  �  �  �  �  �  �  �  �  �  �  �    '  C  E  Z  �  �  �    %  &  '  *  +  ,  3  4  Q  e  �        )  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  a  �  �  �  �  �  �  �  "  #  )  3  8  ?  C  G  H  I  J  L  N      H� $ C:\Progress\OpenEdge\src\adm2\windowmn.i �  f!  C:\Progress\OpenEdge\src\adm2\containr.i �  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i      ��  C:\Progress\OpenEdge\src\adm2\visual.i   \  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    �  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i     I�  C:\Progress\OpenEdge\src\adm2\smart.i    L  Ds   C:\Progress\OpenEdge\gui\fn  �  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �  Q.  C:\Progress\OpenEdge\gui\set �  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i   ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    D  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i     ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i @   �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i �   ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �   V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �   i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i <!  �j  C:\Progress\OpenEdge\gui\get p!  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �!  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �!  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i  "  Su  C:\Progress\OpenEdge\src\adm2\globals.i  T"  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �"  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �"  �  C:\Progress\OpenEdge\src\adm2\appsprto.i #  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   @#  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  �#  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �#  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i  $  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    4$  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   |$  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �$  �   O:\on_in_co\APLIC\vta\r-rep010.w     ,  Q      %       $   (%  �   �      8%  �   �     H%     �     X%  �   {     h%     Y     x%  �   Q     �%     �  #   �%  �   �     �%     �      �%  �   �     �%     �      �%  �   �     �%     �      �%  r   �     &  n   �     &     H  "   (&  i   C     8&     !     H&  P        X&  �   �     h&     �  !   x&  �   �     �&     �     �&  �        �&     ]     �&  �   [     �&     9     �&  g        �&           �&  O   �     '  �   r     '     p      ('  �   @     8'     �     H'  �   �     X'     �     h'  �   �     x'     �     �'  �   �     �'     u     �'  �   t     �'     R     �'  �   A     �'          �'  �        �'     �     (  }   �     (     �     ((     P     8(          H(     �     X(  7   x     h(  �   o     x(  O   a     �(     P     �(          �(  �   �
     �(  �   �
     �(  O   �
     �(     �
     �(     D
     �(  �   
     )  x   
  
   )  M   
     ()     �	     8)     �	     H)  a   �	  
   X)  �  m	     h)     N	     x)  �  	     �)  O   	     �)     �     �)     �     �)  �   �     �)     �     �)     �     �)  x   �     �)     �     *     i     *     e     (*     Q     8*     8     H*  Q   (  
   X*     �     h*     �  
   x*     �     �*     h  
   �*  f   =     �*     �  	   �*  "   �     �*     �     �*     c     �*  Z        �*          +     �     +     �     (+     �     8+     w     H+  '   �       X+     @      h+            x+           