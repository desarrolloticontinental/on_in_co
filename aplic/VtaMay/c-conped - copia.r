	��V�;�a�4  �                                              � 34D4010Butf-8 MAIN d:\newsie\on_in_co\APLIC\vtamay\c-conped - copia.w,, PROCEDURE initializeObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE Carga-Temporal,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER      �<              ��              �� �<  �              �w               &    +   �v �  7   4{ `  8   �~ �   =   � �2  >   � 8  ?   D� X  @   �� l  A           � x  �� �  ? @� f!  iSO8859-1                                                                           �;   $ �                                      �     	             ��                 <  05    d5   3�   $�  $<         ��  �   �<      �<          �                                             PROGRESS                         x           
    
                    �              �                                                                                                     
                                                                                                         �             �             �                                                                                          �  ��                      �                                                                                          �             �         �       �  L  ;     4;  �  ��      `;         �             �6          �7      �   $  �
      �  
    
                  �  T                                                                                                       �
          
  �  �
      L  
    
                  8                �                                                                                          �
          
  |  �
      �  
    
                  �  �  	           h                                                                                          �
          
  (  �
      �  
    
                  �  X  
                                                                                                     �
          
  �  �
      P  
    
                  <               �                                                                                          �
          
  �  �
      �  
    
                  �  �             l                                                                                          �
          
  ,  �
      �  
    
                  �  \                                                                                                       �
          
  �        T  
    
                  @  	             �                                                                                                    
  �	         	                         �  �	             p	                                                                                                      0
        �	                        �	  `
             
                                                                                                      �
  -      X
  
    
                  D
               �
                                                                                          -          
  �  ;        
    
                  �
  �             t                                                                                          ;          
  4  I      �  
    
                  �  d                                                                                                        I          
  �  W      \                        H               �                                                                                          W            �  g                              �  �             x                                                                                          g            8  r      �                        �  h             $                                                                                          r                �      `                        L  4             �                                                                                          �                         INTEGRAL                         PROGRESS                         h     Z  �      Z                         C(�\            Z  ��                              �  �                      `  �  �      CODALMDESCRIPCIONCODCIATDOARTAUTMOVDIRALMHORRECENCALMTELALMCORRSALCORRINGCORRTRFCODDIVCLAVEALMCSGTPOCSGCODCLIFLGREPCAMPO-CALMPRINCIPALALMDESPACHOCAMPO-LOG                                                                        	          
                                                                                                     
                             
        �     d  �      d                         Y|a            m  M                              �  �                      0  �  7_     CODDOCCODCIAFACTORUNDVTACODMATNROPEDCANPEDPREUNIPORDTOIMPDTOIMPLINNROITMAFTIGVAFTISCPREBASPREVTAIMPIGVIMPISCCANATEHORAFLGESTFCHPEDCODAUXMRGUTIPORDTO2TIPVTAPESMATCODCLIALMDESPOR_DSCTOSFLG_FACTORCODDIVCANPICKLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02CANSOLCANAPRIMPDTO2ALMTRFCANTRFLIBRE_D03LIBRE_D04LIBRE_D05LIBRE_F05LIBRE_F03LIBRE_F04SDOCOTCODMATWEBDESMATWEBCANPEDWEBPREUNIWEBIMPLINWEBCUSTOMERARTCODECUSTOMERARTDESCRIPTIONCUSTOMERUNITCODECUSTOMERUNITCODENAMEQTYPRICEROWTOTALCUSTOMERCURRENCYCUSTOMEROLDARTCODEFACTORDESCUENTOTASAIGVIMPORTEUNITARIOSINIMPUESTOIMPORTEREFERENCIALIMPORTEBASEDESCUENTOIMPORTEDESCUENTOIMPORTETOTALSINIMPUESTOMONTOBASEIGVIMPORTEIGVIMPORTETOTALIMPUESTOSIMPORTEUNITARIOCONIMPUESTOCIMPORTEVENTAEXONERADOCIMPORTEVENTAGRATUITOCOTROSTRIBUTOSOPGRATUITOCTIPOAFECTACIONCPREUNISINIMPUESTOCSUMAIMPTETOTALSINIMPUESTOCMONTOBASEIGVCSUMAIGVIMPUESTOBOLSAPLASTICOMONTOTRIBUTOBOLSAPLASTICOCANTIDADBOLSAPLASTICOMONTOUNITARIOBOLSAPLASTICOCIMPORTETOTALCONIMPUESTOIMPORTEBASEDESCUENTONOAFECTOFACTORDESCUENTONOAFECTOIMPORTEDESCUENTONOAFECTO                                                                       	          
                                                                                                                                                                                                                                     !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          �(     �  �      �                         ��{a            �  ��                              �  $                      X!  4  #�     CODCIACODDOCNROPEDFCHPEDCODCLINOMCLIDIRCLIRUCCLIHORAFLGESTCODALMCODMONTPOCMBUSUARIOGLOSAIMPBRTIMPEXOPORIGVIMPDTOIMPTOTIMPIGVCODVENIMPISCIMPVTAFLGSITFMAPGOFCHVENORDCMPCODDIVTIPVTAPORDTOTPOPEDUSRDSCTOLUGENTCODTRANSNROREFCMPBNTEOBSERVANCMPBNTEATENCIONFAXCLILUGENT2IMPFLEFLGIGVTPOLICUBIGEOACUBONNROCARDTIPBONIMPORTEPORCENTUSRAPROBACIONFCHAPROBACIONFCHENTCODPOSFLGENVUSRSACFECSACHORSACLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02USRCHQFCHCHQHORCHQSEDEDIVDESCODREFIMPDTO2GLOSAIMPDTO2FLGIMPODUSRIMPODFCHIMPODUSRACTFECACTHORACTMOTREPOSICIONVTAPUNTUALCROSSDOCKINGALMACENXDCODORIGENNROORIGENDTALMACENDTEMPAQESPECCLIENTE_RECOGELISTA_DE_PRECIOSCUSTOMERPURCHASEORDERCUSTOMERREQUESTORDERTYPEPERIODCURRENCYPAYFORMDELIVERYDATEREGION1REGION1NAMEREGION2REGION2NAMEREGION3REGION3NAMETELEPHONECONTACTRECEPTORCONTACTRECEPTORNAMEDELIVERYADDRESSCUSTOMERLABELOFFICECUSTOMEROFFICECUSTOMERNAMECUSTOMERSTOCKDEPOCUSTOMERSTOCKDEPONAMECONSOLIDATEINVOICECUSTOMERINVOICECUSTOMERGROUPITEMSPESOVOLUMENEMBALAJE_ESPECIALDELIVERYGROUPDNICLIE-MAILIMPPERCEPCIONPORPERCEPCIONCODPAISCODDEPTCODPROVCODDISTREFERENCEADDRESSIDCUSTOMERFLAGMIGRACIONMIGFECHAMIGHORAMIGUSUARIOTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                       	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /         0 
        1          2 
        3         4         5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          P   "       Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �+     �  �      �                         �ɺ[            �  �l                              �  )                       *   )  �       CODCIACLVMODPORIGVDIAS-RESHORA-RESMINU-RESBRRPEDCLIVARTPOCMBITEMS_FACTURAITEMS_BOLETAITEMS_GUIASITEMS_PEDIDOITEMS_N_CREDITOITEMS_N_DEBITODTOMAXDTODISDTOMAYMRGPUBDTOPROITEMS_PEDMOSCLA_COMPRACLA_VENTACODCTAMRGMINMRGMAYMRGDISFACPORALMALTTOLVENROUNDOCUPORMORA                                                                       	          
                                                                                                                                                                                                                                     !          �,      �  �      �                          �ra            �  ��                              �   ,                      d,  0,  1      CODCIATABLACODIGONOMBREVALORCAMPO-CCAMPO-DCAMPO-L                                                                      	          0  !   1   �      1                          �ɺ[            1   .'                              �  D-                      d.  T-  %     CODCIACODALMCODMATUNDVTACODUBISTKACTSTKMINSTKMAXSTKREPSTKINIVINMN1VINMN2VCTMN1VCTMN2FCHINGFCHSALFCHINVSELINVFACEQUDESMATALMDESCODMARCODANTSTKACTCBDLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKCOMPROMETIDOSTOCKMAXSTOCKSEGSTOCKMAXSEG                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          |3  "   :   �      :                          �ɺ[            C   ��                              �  �0                      �1  �0  %     CODCIACODALMTIPMOVNRODOCFCHDOCNROSERFCHVTOALMPEDUSUARIOFECACTHORACTFCHAPRHORAPRUSRACTFECHAHORAUSRAPRFLGESTFLGSITGLOSAOOSERNRVTAPUNTUALLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_D03LIBRE_F03MOTREPOSICIONCROSSDOCKINGALMACENXDCODREFNROREFINCIDENCIA                                                                       	          
                                                                                                                           )                                                                                                            !          "          #          $          %          &              #   ^   �      ^                          Wr_            ^   �Z                              �  �3                      p4  4  a      CODCIACODALMTIPMOVNROSERNRODOCCANREQCODMATITEMCANATENCANTRANALMPEDSTKACTCANAPROCANGENORIGENFLGEST                                                                         	          
                                                                                              ��                                               ��          x6  �6  < �5                         
             
             
                                         
                                                                                                                <   L   \   l   |   �   �   �   �   �   �   �   �         <   L   \   l   |   �   �   �   �   �   �   �   �                                                                                                                                       	                                 09  <9  D9  T9  L9          X9             l9  x9  |9  �9  �9                         �9  �9  �9  �9  �9                         �9  �9  �9  �9  �9           :              :  ,:  8:  L:  @:                         T:  `:  h:  x:  p:          |:             �:  �:  �:  �:  �:                         �:  �:  �:  �:  �:           ;                                                         t-CodAlm    x(3)    Almac�n Almac�n     C�digo de almac�n   t-CodDoc    XXX Codigo  Codigo      t-Nroped    XXX-XXXXXXXX    No. Pedido  Numero!Pedido       t-CodDiv    x(5)    Division    Division    00000   Ingrese el Codigo de Division   t-FchPed    99/99/9999  Fecha   Fch.Pedido  today   t-NomCli    x(35)   Nombre  Cliente     Nombre del Cliente  t-CodMat    X(6)    Codigo Articulo Codigo Articulo     t-Canped    >,>>>,>>9.9999  Cantidad    Cantidad    0   Cantidad despachada �  ���
������   00000�   �     �                 �     i    
 	    �  �  �  �  �  �  �  �    ��                                                                              e          ����                                �  2                 �    9!   !�    ?!   �    G!   ��    O!   �x    O!    �    W!  ! y�    ^!  " �    ^!  # ��    �          undefined                                                               �       �  �   l    �    ��                  �����               |�                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       8     ;          assignFocusedWidget         �      �            LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    $       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    6       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          L       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    X       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    d       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    w       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    #      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    7      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    E      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    U      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    f      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    s      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER        u   ����  �             d   �           p   �          |   �          �   �          �   �          �   �          �   �              � ߱            Z   �����
   �p
                     h�    �  T  �      �       4   �����                 �                      ��                  �  �                  �r�                       �  d  d    �  �        �       4   �����       $  �  8  ���                         @                        � ߱              �  �  �      D      4   ����D      $  �  �  ���                       �  @         t              � ߱        assignPageProperty                              �  h      ��                      �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            changePage                              �  �      ��                  !  "  �              䒕                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �  �      ��                  $  &  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                              ��                            ����                            constructObject                             �  �      ��                  (  -                Tē                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `             ,               �� 
  �             T  
             ��   �             |               �� 
                 �  
         ��                            ����                            createObjects                               �  �      ��                  /  0  �              @�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �  �      ��                  2  4  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            destroyObject                               �  �      ��                  6  7  �              �Ғ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �  �      ��                  9  ;  �              t��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  �      ��                  =  >                �ו                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                                 �      ��                  @  A  $              �ؕ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                                 �      ��                  C  E  $              0��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  <           ��                            ����                            notifyPage                              4        ��                  G  I  L              �>�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d           ��                            ����                            passThrough                             \  D      ��                  K  N  t              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            removePageNTarget                               �  �      ��                  P  S  �              (��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
               �  
             ��                             ��                            ����                            selectPage                                �      ��                  U  W                h]�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  4           ��                            ����                            toolbar                             (        ��                  Y  [  @              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X           ��                            ����                            viewObject                              P   8       ��                  ]  ^  h               ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                P!  8!      ��                  `  b  h!              `�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �!           ��                            ����                            disablePagesInFolder    
      �!       "    �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder  "      L"      �"    �      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  `"      �"      �"          HANDLE, getCallerWindow �"      �"      #          HANDLE, getContainerMode    �"       #      T#    )      CHARACTER,  getContainerTarget  4#      `#      �#    :      CHARACTER,  getContainerTargetEvents    t#      �#      �#    M      CHARACTER,  getCurrentPage  �#      �#      $    f      INTEGER,    getDisabledAddModeTabs  �#      $$      \$     u      CHARACTER,  getDynamicSDOProcedure  <$      h$      �$  !  �      CHARACTER,  getFilterSource �$      �$      �$  "  �      HANDLE, getMultiInstanceActivated   �$      �$       %  #  �      LOGICAL,    getMultiInstanceSupported    %      ,%      h%  $  �      LOGICAL,    getNavigationSource H%      t%      �%  %  �      CHARACTER,  getNavigationSourceEvents   �%      �%      �%  &  �      CHARACTER,  getNavigationTarget �%      �%      0&  '        HANDLE, getOutMessageTarget &      8&      l&  (  )      HANDLE, getPageNTarget  L&      t&      �&  )  =      CHARACTER,  getPageSource   �&      �&      �&  *  L      HANDLE, getPrimarySdoTarget �&      �&      '  +  Z      HANDLE, getReEnableDataLinks    �&      $'      \'  ,  n      CHARACTER,  getRunDOOptions <'      h'      �'  -  �      CHARACTER,  getRunMultiple  x'      �'      �'  .  �      LOGICAL,    getSavedContainerMode   �'      �'      (  /  �      CHARACTER,  getSdoForeignFields �'      $(      X(  0  �      CHARACTER,  getTopOnly  8(      d(      �(  1 
 �      LOGICAL,    getUpdateSource p(      �(      �(  2  �      CHARACTER,  getUpdateTarget �(      �(      )  3  �      CHARACTER,  getWaitForObject    �(      )      H)  4  �      HANDLE, getWindowTitleViewer    ()      P)      �)  5        HANDLE, getStatusArea   h)      �)      �)  6        LOGICAL,    pageNTargets    �)      �)      �)  7  +      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �)      4*      d*  8  8      LOGICAL,INPUT h HANDLE  setCallerProcedure  D*      |*      �*  9  H      LOGICAL,INPUT h HANDLE  setCallerWindow �*      �*      �*  :  [      LOGICAL,INPUT h HANDLE  setContainerMode    �*      +      D+  ;  k      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  $+      l+      �+  <  |      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �+      �+      �+  =  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �+      ,      H,  >  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  (,      x,      �,  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �,      �,       -  @  �      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �,       -      T-  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   4-      t-      �-  B  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �-      �-      .  C  	      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �-      L.      �.  D  #      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   `.      �.      �.  E  7      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �.      /      8/  F  Q      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget /      X/      �/  G  e      LOGICAL,INPUT phObject HANDLE   setPageNTarget  l/      �/      �/  H  y      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �/       0      00  I  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget 0      P0      �0  J  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    d0      �0      �0  K  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �0      1      @1  L  �      LOGICAL,INPUT phObject HANDLE   setRunDOOptions  1      `1      �1  M  �      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  p1      �1      �1  N  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �1      2      @2  O  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields  2      l2      �2  P        LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �2      �2      �2  Q 
       LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �2      3      H3  R  #      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget (3      l3      �3  S  3      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    |3      �3      �3  T  C      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �3      4      L4  U  T      LOGICAL,INPUT phViewer HANDLE   getObjectType   ,4      l4      �4  V  i      CHARACTER,  setStatusArea   |4      �4      �4  W  w      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �5  t5      ��                  �  �  �5              ��a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �6  x6      ��                  �  �  �6              �d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �7  |7      ��                  �  �  �7              ��d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �8  �8      ��                  �  �  �8              D�d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �9  �9      ��                  �  �  �9              d�c                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �9           ��                            ����                            getAllFieldHandles  �4      8:      l:  X  �      CHARACTER,  getAllFieldNames    L:      x:      �:  Y  �      CHARACTER,  getCol  �:      �:      �:  Z  �      DECIMAL,    getDefaultLayout    �:      �:       ;  [  �      CHARACTER,  getDisableOnInit     ;      ,;      `;  \  �      LOGICAL,    getEnabledObjFlds   @;      l;      �;  ]  �      CHARACTER,  getEnabledObjHdls   �;      �;      �;  ^  �      CHARACTER,  getHeight   �;      �;      <  _ 	 �      DECIMAL,    getHideOnInit   �;      $<      T<  `         LOGICAL,    getLayoutOptions    4<      `<      �<  a        CHARACTER,  getLayoutVariable   t<      �<      �<  b        CHARACTER,  getObjectEnabled    �<      �<      =  c  1      LOGICAL,    getObjectLayout �<       =      P=  d  B      CHARACTER,  getRow  0=      \=      �=  e  R      DECIMAL,    getWidth    d=      �=      �=  f  Y      DECIMAL,    getResizeHorizontal �=      �=      �=  g  b      LOGICAL,    getResizeVertical   �=      >      <>  h  v      LOGICAL,    setAllFieldHandles  >      H>      |>  i  �      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    \>      �>      �>  j  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �>      �>      $?  k  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    ?      H?      |?  l  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   \?      �?      �?  m  �      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �?      �?       @  n  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout  @      D@      t@  o  �      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal T@      �@      �@  p  �      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �@      �@      ,A  q  	      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated A      TA      �A  r  #	      LOGICAL,    getObjectSecured    hA      �A      �A  s  7	      LOGICAL,    createUiEvents  �A      �A      B  t  H	      LOGICAL,    bindServer                              �B  �B      ��                  �  �  �B              T�a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �C  �C      ��                  �  �  �C              ��b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �D  �D      ��                  �  �  �D              ��b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �E  �E      ��                  �  �  �E              (�c                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �F  �F      ��                  �  �  �F              ؿc                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �G  �G      ��                  �  �  �G              ��b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �H  �H      ��                  �  �  �H              �b                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �H  
         ��                            ����                            startServerObject                               �I  �I      ��                  �  �  J              ��b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                 K  �J      ��                  �  �  K               Fa                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  0K           ��                            ����                            getAppService   �A      �K      �K  u  W	      CHARACTER,  getASBound  �K      �K       L  v 
 e	      LOGICAL,    getAsDivision   �K      L      <L  w  p	      CHARACTER,  getASHandle L      HL      tL  x  ~	      HANDLE, getASHasStarted TL      |L      �L  y  �	      LOGICAL,    getASInfo   �L      �L      �L  z 	 �	      CHARACTER,  getASInitializeOnRun    �L      �L      (M  {  �	      LOGICAL,    getASUsePrompt  M      4M      dM  |  �	      LOGICAL,    getServerFileName   DM      pM      �M  }  �	      CHARACTER,  getServerOperatingMode  �M      �M      �M  ~  �	      CHARACTER,  runServerProcedure  �M      �M      (N    �	      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   N      lN      �N  �  
      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   |N      �N      �N  �  
      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �N      O      DO  �   
      LOGICAL,INPUT phASHandle HANDLE setASInfo   $O      dO      �O  � 	 ,
      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    pO      �O      �O  �  6
      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �O      P      <P  �  K
      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   P      \P      �P  �  Z
      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  pP      �P      �P  �  l
      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �Q  �Q      ��                  �  �  �Q              �b                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  R             �Q  
             ��   4R              R               �� 
                 (R  
         ��                            ����                            addMessage                               S  S      ��                  �  �  8S              �a                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �S             PS               ��   �S             xS               ��                  �S           ��                            ����                            adjustTabOrder                              �T  �T      ��                  �  �  �T              jc                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
   U             �T  
             �� 
  (U             �T  
             ��                  U           ��                            ����                            applyEntry                              V  �U      ��                  �  �  ,V              T�b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  DV           ��                            ����                            changeCursor                                @W  (W      ��                  �  �  XW              �c                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  pW           ��                            ����                            createControls                              lX  TX      ��                  �  �  �X              t�c                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               pY  XY      ��                  �  �  �Y               �c                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                tZ  \Z      ��                  �  �  �Z               �d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �[  h[      ��                  �  �  �[              ��d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �\  h\      ��                  �  �  �\              X�d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �]  h]      ��                  �  �  �]              ��b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �^  p^      ��                  �  �  �^              <�b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �_  x_      ��                  �  �  �_              2c                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �_             �_  
             ��   `             �_               ��   D`             `               ��                  8`           ��                            ����                            modifyUserLinks                             4a  a      ��                  �  �  La              ��d                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �a             da               ��   �a             �a               �� 
                 �a  
         ��                            ����                            removeAllLinks                              �b  �b      ��                  �  �  �b              ��a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �c  �c      ��                  �  �  �c              hja                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  d             �c  
             ��   <d             d               �� 
                 0d  
         ��                            ����                            repositionObject                                0e  e      ��                  �  �  He              4!c                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �e             `e               ��                  �e           ��                            ����                            returnFocus                             �f  hf      ��                  �  �  �f              l�c                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �f  
         ��                            ����                            showMessageProcedure                                �g  �g      ��                  �  �  �g              �'c                    O   ����    e�          O   ����    R�          O   ����    ��            ��   h             �g               ��                  h           ��                            ����                            toggleData                              i  �h      ��                  �    i              ,a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  4i           ��                            ����                            viewObject                              ,j  j      ��                      Dj              �a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �P      �j      �j  � 
 �      LOGICAL,    assignLinkProperty  �j      �j      k  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �j      `k      �k  �  �      CHARACTER,  getChildDataKey pk      �k      �k  �  �      CHARACTER,  getContainerHandle  �k      �k      l  �        HANDLE, getContainerHidden  �k      l      Hl  �         LOGICAL,    getContainerSource  (l      Tl      �l  �  3      HANDLE, getContainerSourceEvents    hl      �l      �l  �  F      CHARACTER,  getContainerType    �l      �l      m  �  _      CHARACTER,  getDataLinksEnabled �l      m      Lm  �  p      LOGICAL,    getDataSource   ,m      Xm      �m  �  �      HANDLE, getDataSourceEvents hm      �m      �m  �  �      CHARACTER,  getDataSourceNames  �m      �m      n  �  �      CHARACTER,  getDataTarget   �m      n      @n  �  �      CHARACTER,  getDataTargetEvents  n      Ln      �n  �  �      CHARACTER,  getDBAware  `n      �n      �n  � 
 �      LOGICAL,    getDesignDataObject �n      �n      �n  �  �      CHARACTER,  getDynamicObject    �n      o      8o  �  �      LOGICAL,    getInstanceProperties   o      Do      |o  �        CHARACTER,  getLogicalObjectName    \o      �o      �o  �  !      CHARACTER,  getLogicalVersion   �o      �o       p  �  6      CHARACTER,  getObjectHidden �o      p      <p  �  H      LOGICAL,    getObjectInitialized    p      Hp      �p  �  X      LOGICAL,    getObjectName   `p      �p      �p  �  m      CHARACTER,  getObjectPage   �p      �p      �p  �  {      INTEGER,    getObjectParent �p      q      4q  �  �      HANDLE, getObjectVersion    q      <q      pq  �  �      CHARACTER,  getObjectVersionNumber  Pq      |q      �q  �  �      CHARACTER,  getParentDataKey    �q      �q      �q  �  �      CHARACTER,  getPassThroughLinks �q       r      4r  �  �      CHARACTER,  getPhysicalObjectName   r      @r      xr  �  �      CHARACTER,  getPhysicalVersion  Xr      �r      �r  �  �      CHARACTER,  getPropertyDialog   �r      �r      �r  �        CHARACTER,  getQueryObject  �r      s      4s  �  !      LOGICAL,    getRunAttribute s      @s      ps  �  0      CHARACTER,  getSupportedLinks   Ps      |s      �s  �  @      CHARACTER,  getTranslatableProperties   �s      �s      �s  �  R      CHARACTER,  getUIBMode  �s      t      0t  � 
 l      CHARACTER,  getUserProperty t      <t      lt  �  w      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    Lt      �t      �t  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles �t      �t       u  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty     u      Du      tu  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry Tu      �u      �u  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �u      Hv      xv  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    Xv      �v      �v  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  �v      �v      $w  �  �      CHARACTER,  setChildDataKey w      0w      `w  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  @w      �w      �w  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  �w      �w      x  �        LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �w      0x      lx  �  !      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled Lx      �x      �x  �  :      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   �x      �x      y  �  N      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �x      <y      py  �  \      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  Py      �y      �y  �  p      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   �y      �y      $z  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents z      Hz      |z  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  \z      �z      �z  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject �z      �z       {  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject     {      H{      |{  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   \{      �{      �{  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    �{      �{      ,|  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   |      H|      ||  �         LOGICAL,INPUT cVersion CHARACTER    setObjectName   \|      �|      �|  �        LOGICAL,INPUT pcName CHARACTER  setObjectParent �|      �|       }  �         LOGICAL,INPUT phParent HANDLE   setObjectVersion     }      @}      t}  �  0      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    T}      �}      �}  �  A      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks �}      �}      ,~  �  R      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   ~      L~      �~  �  f      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  d~      �~      �~  �  |      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �~      �~      ,  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks         T      �  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   h      �      �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  �      �      8�  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      X�      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage h�      Ȁ      �  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   Ԁ      �      D�  � 	 �      CHARACTER,INPUT pcName CHARACTER    <�      ��   �      �      4   �����                �                      ��                    H                  �a                         ��          ,�  ��      �      4   �����                ��                      ��                    G                  8a                         <�  ��    4  Ԃ  P�      �      4   �����                `�                      ��                  @  B                  �a                       @  �         A                                  �     
                    � ߱        �  $  D  ��  ���                           $  F  �  ���                                                � ߱        H�    L  X�  Ԅ            4   ����                �                      ��                  M  	                  p	a                       M  h�  �  o   P      ,                                 p�  $   Q  D�  ���                       �  @         t              � ߱        ��  �   R  �      ��  �   S        ��  �   U  �      ��  �   W        ԅ  �   Y  x      �  �   [  �      ��  �   \  h      �  �   ]  �      $�  �   `        8�  �   b  �      L�  �   c        `�  �   e  �      t�  �   f   	      ��  �   g  <	      ��  �   h  �	      ��  �   i  ,
      Ć  �   o  h
      ؆  �   q  �
      �  �   w         �  �   y  �      �  �   {         (�  �   |  |      <�  �   �  �      P�  �   �  l      d�  �   �  �      x�  �   �  \      ��  �   �  �      ��  �   �        ��  �   �  �      ȇ  �   �  �      ܇  �   �  0      ��  �   �  l      �  �   �  �      �  �   �  �      ,�  �   �         @�  �   �  �      T�  �   �  �      h�  �   �        |�  �   �  P      ��  �   �  �      ��  �   �  �      ��  �   �        ̈  �   �  @      ��  �   �  |          �   �  �                      �          x�  `�      ��                  8	  f	  ��              �5c                    O   ����    e�          O   ����    R�          O   ����    ��      (     
                �                     �                         � ߱        8�  $ L	  ��  ���                           O   d	  ��  ��  �               ��          ��  ��    ��                                             ��                            ����                                l4      �      P�     6     ��                      V ��  i                     �    �	  d�  ��             4   ����                 ��                      ��                  �	  
                  ��b                       �	  t�  �  �   �	  `      �  �   �	  �      ,�  �   �	  P      @�  �   �	  �      T�  �   �	  H      h�  �   �	  �      |�  �   �	  8      ��  �   �	  �      ��  �   �	  0      ��  �   �	  �      ̌  �   �	         ��  �   �	  �      �  �   �	            �   �	  �      ��    
  $�  ��            4   ����                ��                      ��                  
  �
                  �aa                       
  4�  č  �   
  d      ؍  �   
  �      �  �   
  L       �  �   
  �      �  �   
  <      (�  �    
  �      <�  �   !
  ,       P�  �   "
  �       d�  �   #
  !      x�  �   $
  �!      ��  �   %
  "      ��  �   &
  x"      ��  �   '
  �"      Ȏ  �   (
  h#      ܎  �   )
  �#      ��  �   *
  `$      �  �   +
  �$      �  �   ,
  X%      ,�  �   -
  �%      @�  �   .
  P&      T�  �   /
  �&      h�  �   0
  H'      |�  �   1
  �'      ��  �   2
  @(      ��  �   3
  �(      ��  �   4
  8)      ̏  �   5
  �)          �   6
  0*      ��    �
  ��  x�      �*      4   �����*                ��                      ��                  �
  e                  �%d                       �
  �  ��  �   �
  �*      ��  �   �
  t+      Đ  �   �
  �+      ؐ  �   �
  d,      �  �   �
  �,       �  �   �
  L-      �  �   �
  �-      (�  �   �
  �-      <�  �   �
  p.      P�  �   �
  �.      d�  �   �
  �.      x�  �   �
  \/      ��  �   �
  �/      ��  �   �
  L0      ��  �   �
  �0      ȑ  �   �
  41      ܑ  �   �
  �1      �  �   �
  $2      �  �   �
  �2      �  �   �
  �2      ,�  �   �
  P3      @�  �   �
  �3      T�  �   �
  84      h�  �   �
  t4      |�  �   �
  �4      ��  �   �
  ,5      ��  �   �
  h5      ��  �   �
  �5      ̒  �   �
  �5      ��  �   �
  6      ��  �   �
  X6      �  �   �
  �6      �  �   �
  �6      0�  �   �
  D7      D�  �   �
  �7      X�  �   �
  �7      l�  �   �
  �7      ��  �   �
  48      ��  �   �
  p8      ��  �   �
  �8      ��  �   �
  �8      Г  �   �
  \9      �  �   �
  �9      ��  �   �
  D:      �  �   �
  �:       �  �   �
  4;      4�  �   �
  �;      H�  �   �
  ,<      \�  �   �
  �<      p�  �   �
  $=      ��  �   �
  �=      ��  �   �
  �=      ��  �   �
  X>      ��  �   �
  �>      Ԕ  �   �
  �>      �  �   �
  ?          �   �
  �?      T�  $  q  (�  ���                       �?     
                    � ߱        �    �  p�  ��      �?      4   �����?      /   �  ��     ��                          3   ����@            ܕ                      3   ����,@  @�    �  �  ��  p�  H@      4   ����H@  	              ��                      ��             	     �  9                  |rb                       �  �  ��  �   �  �@       �  $  �  Ԗ  ���                       �@     
                    � ߱        �  �   �  �@      l�  $   �  @�  ���                       A  @         A              � ߱        (�  $  �  ��  ���                       pA       	       	           � ߱        �A     
                `B                     �C  @        
 pC              � ߱        ��  V   �  ė  ���                        �C       	       	       �C       
       
       ,D       	       	           � ߱        H�  $  �  T�  ���                       �D     
                hE                     �F  @        
 xF              � ߱        ؙ  V   �  �  ���                        �F     
                @G                     �H  @        
 PH              � ߱            V     t�  ���                        
              8�                      ��             
     ;  �                  tb                       ;  �  �H     
                 I                     pJ  @        
 0J          �J  @        
 �J          8K  @        
 �J          �K  @        
 XK              � ߱            V   P  ��  ���                        adm-clone-props �  d�              �     7     `                          \  $                     start-super-proc    t�  Л  �           �     8                                  E                     ؜    �  \�  l�      $O      4   ����$O      /   �  ��     ��                          3   ����4O            Ȝ                      3   ����TO  0�  $    �  ���                       tO                         � ߱        �      L�  ȝ  h�  �O      4   �����O                <�                      ��                                       �,a                         \�  �O                     �O                     �O                         � ߱            $    ؝  ���                             !  ��  ��      �O      4   �����O  P                         � ߱            $  "  ��  ���                       �    )  �  �  p�  P      4   ����P      $  *  D�  ���                       8P                         � ߱            �   G  LP      �P     
                Q                     XR  @        
 R              � ߱        �  V   [  ��  ���                        (�  �   �  dR      ��      D�  T�      �R      4   �����R      /     ��     ��                          3   �����R            ��                      3   �����R  |�  $    �  ���                       �R                         � ߱        S     
                �S                     �T  @        
 �T              � ߱        ��  V     �  ���                        ��    �  ġ  @�      �T      4   �����T                P�                      ��                  �  �                  �Ec                       �  ԡ      g   �  h�         ~�,�                           0�           �  �      ��                  �      �              Fc                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  \�     l�  U                      3   ����U  ��     
   ��                      3   ����(U         
   ��                      3   ����0U    ��                              ��        e                  ����                                        |�              9      ̣                      g                               ��  g   �  ��          ~�	4�                           h�          8�   �      ��                  �  �  P�              �Fc                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  TU                      3   ����8U            ĥ                      3   ����\U    ��                              ��        e                  ����                                        ��              :      ԥ                      g                               ��  g   �  ��          ~�	<�                           p�          @�  (�      ��                  �  �  X�              LGc                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  �U                      3   ����xU            ̧                      3   �����U    ��                              ��        e                  ����                                        ��              ;      ܧ                      g                               ��    �  ��  0�      �U      4   �����U                @�                      ��                  �  �                  �c                       �  Ĩ  ��  /   �  l�     |�                          3   �����U            ��                      3   �����U  ��  /  �  ة     �  $V                      3   ����V  �     
   �                      3   ����,V  H�        8�                      3   ����4V  x�        h�                      3   ����HV            ��                      3   ����lV  Ы    �  Ī  Ԫ      �V      4   �����V      /  �   �     �  W                      3   �����V  @�     
   0�                      3   ���� W  p�        `�                      3   ����(W  ��        ��                      3   ����<W            ��                      3   ����`W        �  �  ��      �W      4   �����W      /  �  (�     8�  �W                      3   �����W  h�     
   X�                      3   �����W  ��        ��                      3   �����W  Ȭ        ��                      3   �����W            �                      3   ����X  ��     �  8X                                     LX     
                �X                     Z  @        
 �Y              � ߱         �  V   W  ,�  ���                        ,Z     
                �Z                     �[  @        
 �[              � ߱        ��  V   ~  ��  ���                         \  @         \          H\  @         4\              � ߱        ��  $   �  L�  ���                       t�  g   �  خ         ~6�                            ��          p�  X�      ��                  �  �  ��              �ga                    O   ����    e�          O   ����    R�          O   ����    ��            �  \\  }        ��                              ��        e                  ����                                        �              <      ��                      g                               ı    �  ��  �      t\      4   ����t\                �                      ��                  �  �                  6�                       �  ��  `�  	  �  P�                                        3   �����\  ��  /   �  ��                                 3   �����\  ��  �   �  ]      O   �  ��  ��  ]  H�    �  �  �      0]      4   ����0]      $   �  �  ���                       �]  @         t]              � ߱        �  /     t�                                 3   �����]                0�          �   �      ��                   
                  �6�                ��       ��      O       ��          O       ��      l�  /     \�                                 3   �����]      k   	  ��                    %�        �       /     ̳                                 3   �����]  adm-create-objects  �  ܳ                      =      �                               /                     Carga-Temporal  �  L�              0     >     �0                          �0  �                      disable_UI  \�  ��                      ?      �                               �   
                   enable_UI   Ĵ   �                      @      �             4              �   	                   initializeObject    ,�  ��                      A      ,                              �                       ���  ���  �           0�  8   ����#   @�  8   ����#   P�  8   ����"   `�  8   ����"   p�  8   ����!   ��  8   ����!   ��  8   ����    ��  8   ����    ��  8   ����   ȶ  8   ����   ض    �  8   ����   �  8   ����          �  8   ����   �  8   ����    �  8   ����   0�  8   ����       8   ����       8   ����       P�  \�      toggleData  ,INPUT plEnabled LOGICAL    @�  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  x�  �  �      returnFocus ,INPUT hTarget HANDLE   Է  �  ,�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    �  h�  t�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE X�  ȸ  ظ      removeAllLinks  ,   ��  �  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ܸ  T�  h�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    D�  �  �      hideObject  ,   й   �  �      exitObject  ,   �   �  8�      editInstanceProperties  ,   �  L�  \�      displayLinks    ,   <�  p�  ��      createControls  ,   `�  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  к  ܺ      applyEntry  ,INPUT pcField CHARACTER    ��  �  �      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  p�  |�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER `�  Ի  ܻ      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE Ļ  0�  @�      unbindServer    ,INPUT pcMode CHARACTER  �  h�  |�      startServerObject   ,   X�  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  ̼  �      restartServerObject ,   ��  ��  �      initializeServerObject  ,   �   �  4�      disconnectObject    ,   �  H�  \�      destroyServerObject ,   8�  p�  |�      bindServer  ,   `�  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  ̽  ܽ      enableObject    ,   ��  �   �      disableObject   ,   �  �   �      applyLayout ,   �  4�  @�      viewPage    ,INPUT piPageNum INTEGER    $�  l�  x�      viewObject  ,   \�  ��  ��      toolbar ,INPUT pcValue CHARACTER    |�  ��  ̾      selectPage  ,INPUT piPageNum INTEGER    ��  ��  �      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER �  H�  T�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  8�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  п  ܿ      initPages   ,INPUT pcPageList CHARACTER ��  �  $�      initializeVisualContainer   ,   ��  8�  D�      hidePage    ,INPUT piPageNum INTEGER    (�  p�  ��      destroyObject   ,   `�  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  ��  ��      createObjects   ,   ��  ��   �      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  t�  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  d�  ��  ��      changePage  ,   ��  ��  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
   "      "      "      "      "      "      "  	        
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              � 
"    
 �%              � ��  �         �      \     H     $              
�    � �   �     
�             �G� �   �G     
�             �G                      
�            � �     
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        h    7%               
"   
 �           �    1�   
 �    �%               o%   o           �     
"   
 �               1�    �    �%               o%   o           � -   
"   
 �           �    1� 4  
 �    �%               o%   o           � ?   
"   
 �           �    1� K   �    �%               o%   o           � Y  
 
"   
 �           l    1� d   �    �%               o%   o           � s   
"   
 �           �    1� �   � �   �%               o%   o           %               
"   
 ��          \    1� �   �� �     
"   
 �           �    1� �   �    �%               o%   o           � �  e 
"   
 �               1� .   �    �%               o%   o           � =  ? 
"   
 �           �    1� }   � �   �%               o%   o           %               
"   
 �           �    1� �   � �   �%               o%   o           %               
"   
 �           x    1� �   � �   �%               o%   o           %              
"   
 ��          �    1� �   �� �     
"   
 �           0	    1� �  
 � �   �%               o%   o           %               
"   
 �           �	    1� �   �    �%               o%   o           �     
"   
 ��           
    1� �   �� �     
"   
 �           \
    1� �   �    �%               o%   o           � �  t 
"   
 ��          �
    1� i  
 �� �     
"   
 �               1� t   �    �%               o%   o           � �  � 
"   
 �           �    1�    �    �%               o%   o           �     
"   
 �           �    1� )  
 � 4   �%               o%   o           %               
"   
 c�           p    1� 8   c� �   �%               o%   o           %               
"   
 b�           �    1� @   b�    �%               o%   o           �     c
"   
 b�           `    1� Q   b�    �%               o%   o           o%   o           
"   
 ��           �    1� a  
 ��    �%               o%   o           �     b
"   
 b�           P    1� l   b� }  	 �%               o%   o           � �  / �
"   
 ��          �    1� �   �� }  	   
"   
 b�                1� �   b� }  	 �o%   o           o%   o           �     b
"   
 ��          t    1� �   �� }  	   
"   
 a�           �    1� �   a� }  	 �o%   o           o%   o           �     a
"   
 ��          $    1� �   �� �     
"   
 ��          `    1� 	   �� }  	   
"   
 ��          �    1�    �� }  	   
"   
 ��          �    1� #   �� }  	   
"   
 a�               1� 1   a� �   �o%   o           o%   o           %              
"   
 ��          �    1� B   �� }  	   
"   
 ��          �    1� P  
 �� [     
"   
 ��              1� c   �� }  	   
"   
 ��          D    1� r   �� }  	   
"   
 ��          �    1� �   �� }  	   
"   
 ��          �    1� �   �� }  	   
"   
 ��          �    1� �  	 �� }  	   
"   
 ��          4    1� �   �� }  	   
"   
 ��          p    1� �   �� }  	   
"   
 b�           �    1� �   b�    �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 a
"   
   
"   
 �(�  L ( l       �        t    �� �   � P   �        �    �@    
� @  , 
�       �    �� �     p�               �L
�    %              � 8      �    � $         � �          
�    �      
"   
 �� @  , 
�       �    �� 4  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 a�           T    1�   
 a�    �%               o%   o           �     a
"   
 a�           �    1� !  
 a�    �%               o%   o           o%   o           
"   
 c�           D    1� ,   c� �   �%               o%   o           o%   o           
"   
 b�           �    1� 5   b� �   �%               o%   o           %               
"   
 c�           <    1� D   c� �   �%               o%   o           %               
"   
 d�           �    1� Q   d�    �%               o%   o           �     c
"   
 a�           ,    1� X   a� �   �%               o%   o           %              
"   
 a�           �    1� j   a� �   �%               o%   o           o%   o           
"   
 ��           $    1� v   ��    �%               o%   o           o%   o           
"   
 c�           �    1� �  	 c�    �%               o%   o           �     b
"   
 c�               1� �   c�    �%               o%   o           o%   o           
"   
 c�           �    1� �   c�    �%               o%   o           o%   o           
"   
 c�               1� �   c� �   �%               o%   o           %               
"   
 c�           �    1� �   c� �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 a�           X    1� �   a� }  	 �%               o%   o           �     a
"   
 ��           �    1� �   �� }  	 �%               o%   o           �     a
"   
 a�           @    1� �   a� �   �%               o%   o           %               
"   
 d�           �    1� �   d� }  	 �%               o%   o           �     a
"   
 c�           0    1�    c� }  	 �%               o%   o           �     d
"   
 a�           �    1�    a� �   �%               o%   o           %               
"   
 b�                 1� !   b� }  	 �%               o%   o           �     a
"   
 c�           �     1� 0   c� }  	 �%               o%   o           �     b
"   
 a�           !    1� ?   a� }  	 �%               o%   o           �     c
"   
 a�           |!    1� M   a� }  	 �%               o%   o           o%   o           
"   
 a�           �!    1� [   a� }  	 �%               o%   o           �     �
"   
 d�           l"    1� k   d� }  	 �%               o%   o           �     a
"   
 c�           �"    1� y  	 c� [   �%               o%   o           %               
"   
 a�           \#    1� �   a� [   �%               o%   o           %               
"   
 a�           �#    1� �   a� �   �%               o%   o           o%   o           
"   
 b�           T$    1� �   b� �   �%               o%   o           o%   o           
"   
 a�           �$    1� �   a� �   �%               o%   o           %               
"   
 ��           L%    1� �   �� �   �%               o%   o           %               
"   
 a�           �%    1� �   a� �   �%               o%   o           %               
"   
 d�           D&    1� �   d� �   �%               o%   o           %       
       
"   
 d�           �&    1� �   d� �   �%               o%   o           o%   o           
"   
 c�           <'    1�     c� �   �%               o%   o           %              
"   
 c�           �'    1�    c� �   �%               o%   o           o%   o           
"   
 b�           4(    1�    b� �   �%               o%   o           %              
"   
 b�           �(    1� %   b� �   �%               o%   o           o%   o           
"   
 ��           ,)    1� 2   �� �   �%               o%   o           %              
"   
 ��           �)    1� :   �� �   �%               o%   o           o%   o           
"   
 d�           $*    1� B   d� }  	 �%               o%   o           �     cP �L 
�H T   %              �     }        �GG %              
"   
 c�           �*    1� T   c� 4   �%               o%   o           %               
"   
 c�           h+    1� `   c� 4   �%               o%   o           o%   o           
"   
 a�           �+    1� l   a�    �%               o%   o           �     c
"   
 b�           X,    1� |   b�    �%               o%   o           � �  - a
"   
 a�           �,    1� �   a�    �%               o%   o           �     b
"   
 ��           @-    1� �   ��    �%               o%   o           � �   a
"   
 ��          �-    1�    �� �     
"   
 c�           �-    1� #   c�    �%               o%   o           �     a
"   
 ��          d.    1� /  
 �� �     
"   
 ��          �.    1� :   �� �     
"   
 a�           �.    1� G   a� }  	 �%               o%   o           �     c
"   
 b�           P/    1� T   b�    �%               o%   o           �     a
"   
 b�           �/    1� a   b� �   �%               o%   o           o%   o           
"   
 ��           @0    1� n   ��    �%               o%   o           � �  ! b
"   
 c�           �0    1� �   c�    �%               o%   o           �     �
"   
 d�           (1    1� �   d�    �%               o%   o           � �   c
"   
 d�           �1    1� �  	 d� 4   �%               o%   o           o%   o           
"   
 c�           2    1� �   c� �   �%               o%   o           %               
"   
 ��          �2    1� �   �� �     
"   
 b�           �2    1� �   b�    �%               o%   o           � 
   a
"   
 b�           D3    1�    b� }  	 �%               o%   o           �     b
"   
 ��           �3    1� &   �� }  	 �%               o%   o           �     b
"   
 ��          ,4    1� 6   �� �     
"   
 ��          h4    1� H   �� }  	   
"   
 d�           �4    1� [   d� �   �o%   o           o%   o           %               
"   
 ��           5    1� r   �� �     
"   
 ��          \5    1� �   �� }  	   
"   
 ��          �5    1� �   �� }  	   
"   
 ��          �5    1� �   �� }  	   
"   
 ��          6    1� �   �� }  	   
"   
 ��          L6    1� �   �� }  	   
"   
 ��          �6    1� �   �� �     
"   
 ��           �6    1� �   ��    �%               o%   o           �   4 c
"   
 ��          87    1� :   �� �     
"   
 ��          t7    1� G   �� �     
"   
 ��          �7    1� W   �� �     
"   
 ��          �7    1� d   �� }  	   
"   
 ��          (8    1� x   �� }  	   
"   
 ��          d8    1� �   �� }  	   
"   
 ��          �8    1� �   �� �     
"   
 a�           �8    1� �   a� }  	 �%               o%   o           �     b
"   
 c�           P9    1� �   c� }  	 �%               o%   o           �     a
"   
 c�           �9    1� �   c� }  	 �%               o%   o           �     c
"   
 ��           8:    1� �   �� }  	 �%               o%   o           �     c
"   
 a�           �:    1� �   a� �   �%               o%   o           %               
"   
 a�           (;    1� �   a� �   �%               o%   o           o%   o           
"   
 b�           �;    1�    b� �   �%               o%   o           %               
"   
 b�            <    1�    b� �   �%               o%   o           %               
"   
 b�           �<    1� )   b� �   �%               o%   o           o%   o           
"   
 c�           =    1� D   c� �   �%               o%   o           %               
"   
 ��          �=    1� R   �� }  	   
"   
 c�           �=    1� `   c� �   �%               o%   o           %              
"   
 ��          L>    1� q   �� }  	   
"   
 ��          �>    1� }   �� }  	   
"   
 ��          �>    1� �  
 �� }  	   
"   
 b�            ?    1� �   b� }  	 �%               o%   o           � �   c
"   
 a�           t?    1� �   a� }  	 �%               o%   o           �     b
�             �G "    �%     start-super-proc ��%     adm2/smart.p ~�P �L 
�H T   %              �     }        �GG %              
"   
   �       �@    6� �     
"   
   
�        �@    8
"   
   �        �@    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        0B    �� �   � P   �        <B    �@    
� @  , 
�       HB    �� �   �p�               �L
�    %              � 8      TB    � $         � �          
�    �    �
"   
 �p� @  , 
�       dC    �� �   �p�               �L"  	  , �   � �   c� �   ��     }        �A      |    "  	    � �   a%              (<   \ (    |    �     }        �A� �   �A"  
  c    "  	  �"  
  c  < "  	  �"  
  c(    |    �     }        �A� �   �A"  
  c
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        8E    �� �   � P   �        DE    �@    
� @  , 
�       PE    �� �   �p�               �L
�    %              � 8      \E    � $         � �          
�    �    �
"   
 �p� @  , 
�       lF    ��   
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 d(�  L ( l       �        G    �� �   � P   �        G    �@    
� @  , 
�       (G    �� �   �p�               �L
�    %              � 8      4G    � $         � �   �     
�    �    �
"   
 �p� @  , 
�       DH    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 b
"   
   
"   
   (�  L ( l       �        �H    �� �   � P   �        �H    �@    
� @  , 
�       I    �� �     p�               �L
�    %              � 8      I    � $         � �          
�    �      
"   
 �p� @  , 
�       $J    �� 4  
 �p�               �L%     SmartDialog 
"   
   p� @  , 
�       �J    �� K     p�               �L% 
    DIALOG-BOX  
"   
  p� @  , 
�       �J    �� �    p�               �L%               
"   
  p� @  , 
�       LK    �� �    p�               �L(        �       �       �       �     }        �A
�H T   %              �     }        �GG %              
"   
 b (   � 
"   
 �    �        ,L    �� �   �
"   
   � 8      xL    � $         � �          
�    �    �
"   
   �        �L    �
"   
   �       �L    /
"   
   
"   
   �       M    6� �     
"   
   
�        HM    8
"   
   �        hM    �
"   
   �       �M    �
"   
   p�    �    a
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        LN    �A"    �A
"   
   
�        �N    �@ � 
"   
 b"      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p �a�    � �     
�    �     }        �%               %      Server  - �     }        �    "    ��     �%                   "    ��     �%      NONE    p�,  8         $     "    d        � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   �p�               �L
�    %              � 8      �P    � $         � �          
�    �    �
"   
 �p� @  , 
�       R    �� �   �p�               �L"    , p�,  8         $     "    d        � �   �
�     "    �%     start-super-proc ��%     adm2/visual.p ��   � �     � �     � �     
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        hS    �� �   � P   �        tS    �@    
� @  , 
�       �S    �� �   �p�               �L
�    %              � 8      �S    � $         � �          
�    �    �
"   
 �p� @  , 
�       �T    �� !   �p�               �L"    , � 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP ~�%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents c%      initializeDataObjects c0 0   A    �    � 8   c
�    � J   �A    �    � 8     
�    � V   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents c%     buildDataRequest ent0 A    �    � 8   �
�    � s   c%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 c(�  L ( l       �        �X    �� �   � P   �        �X    �@    
� @  , 
�       �X    �� �   �p�               �L
�    %              � 8      �X    � $         � �   �     
�    �    �
"   
 �p� @  , 
�       �Y    �� 6   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        xZ    �� �   � P   �        �Z    �@    
� @  , 
�       �Z    �� �   �p�               �L
�    %              � 8      �Z    � $         � �   �     
�    �    �
"   
 �p� @  , 
�       �[    �� �   �p�               �L%              �             I%               �             �%              % 	    END-ERROR a�     }        � `     @     ,         � �  (   G %       
       � �  &   G %       
       �   & �% 
    disable_UI 
�    %                0   � 
�        
�             � 
%   
           
�             � 
�    %     createObjects   %     initializeObject �%     destroyObject   %               �    }        �� R     � b   �%              "       &    &    P     4   %              $    4         %       	       &        "      &    � �   �� �   �"     �"    �"     �&    &    &    &    &    &    &    &    &    &    h    L    0        %              %              %              %              %              � �   �%               "      "  !    "      "      &    &    &    &    &    &    &    &    h ,   L    0        %              %              %              %                  S    "      &    &    "      "      "      "      "      "      "           "          "      "           "     "  	    � �   �� �   �%               "       "      "       &    &    &    &    &    &    &    &    h ,   L    0        %              %              %              %                  S    "      &    &    � �   �"    �"  !  �"    �"    �&    &    &    &    &    &    &    &    h    L    0        %              %              %              %                  "      &    "      "      "      "      "      "      "           "          "      "           "     "  	    � �   �� �   �"     �"    �"     �&    &    &    &    &    &    &    &    &    &    h    L    0        %              %              %              %              %              � �   �"    �"  !  �"    �"    �&    &    &    &    &    &    &    &    h    L    0        %              %              %              %                  "      &    "      "      "      "      "      "      "           "          "      "           "     "  	    "     �&    &     t (    D (   (        "      %              %                 "      %                 "      %       <       � �   �� �   �"     �"    �"     �&    &    &    &    &    &    &    &    &    &    h    L    0        %              %              %              %              %              � �   �"    �"  !  �"    �"    �&    &    &    &    &    &    &    &    h    L    0        %              %              %              %                  "      &     *    4            +  "      %              %              �        "      C   \ \   @    <    "  
    %              %              %             @    <    "  
    %              %              %       <           "    a%                   "    a"    �"      "      "      "      "      "      "          "      "           "      "  	    �       %              "       &    &    &    &    0 4       %              %              $    4      	    %              &    � m      8 8   $    4          %              � m      $    4          %              �       $    4          %              � !              C  � '    �"      %               $    4          %              %               $    4          %              %             � -    �� �   �"     �"    �"     �&    &    &    &    &    &    &    &    &    &    h    L    0        %              %              %              %              %              � �   �"    �"  !  �"    �"    �"     �&    &    &    &    &    &    &    &    �    h    L    0        %              %              %              %                  "      &        "      &    4            +  "      %              %              �        "      C   \ \   @    <    "  
    %              %              %             @    <    "  
    %              %              %       <           "    a%                   "    a"    �"      "      "      "      "      "           "          "      "      � b   �%              "       &    &    P     4   %              $    4         %       	       &        "      &    "     �"    �"     �&    &    &    &    &    &    0        %              %              %              � �   �"     �"    �&    &    &    &    &    &    0        %              %              %              " "   �" "   �" "   �" "   �" "   �"     �&    &    &    &    &    &    &    &    &    &    &    &    �     �    h    L    0        %              %              %              %              %              %                  " # 
    " #     <      S    " "     � g      %               "      (        " "     � s      � s      � w                 " "    � {          " "     �       "      " "     " "     "           " #     " # 
         "      "  	    �    }        �� m      "    b"    b%     CARGA-TEMPORAL  %      SUPER                   �           �   l       ��                 H  l  �               ��d                    O   ����    e�          O   ����    R�          O   ����    ��        $  W  �   ���                       �K     
                    � ߱              X  (  �      8L      4   ����8L                �                      ��                  Y  k                  ��c                       Y  8  �  �  Z  �L            \  �  `      �L      4   �����L                p                      ��                  ]  j                  �c                       ]  �  �  o   ^      ,                                 �  �   _  �L      �  �   `  (M      $  $  a  �  ���                       TM     
                    � ߱        8  �   b  tM      L  �   c  �M      `  �   f  �M          $   i  �  ���                       �M  @         �M              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               h�c                    O   ����    e�          O   ����    R�          O   ����    ��      4                      �          �  $  �    ���                       8N     
                    � ߱                  �  �                      ��                   �  �                  4�a                     �  4      4   ����XN      $  �  �  ���                       �N     
                    � ߱        �    �  4  D      �N      4   �����N      /  �  p                               3   �����N  �  �   �  �N          O   �  ��  ��  O                               , �                          
                               �      ��                            ����                                                        �   l       ��                      �               X7�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��2               $     �                �                    O   ����    e�          O   ����    R�          O   ����    ��        $  .  �   ���                       �]                         � ߱        �  �   1  �]            �      �                   ��                  2  �  0              :d                �&     2         �         ��                            7   ����          ��               X^    �            h                  6   2        �   ��         �  X^    �            h                                                        ^   (^   <^                 �  �           H^           P^                      �   �        O   ����  e�          O   ����  R�          O   ����  ��            �      ,  l2      �  �      ��                  5  F                �c                �     5  H  T  �  @       ��                            7   ����          ��               X_    �            �                  6   5        �   ��         �  X_    �            �                                                        �^   �^   �^   �^   �^                 @  4           _  _  (_  8_  H_           _   _  0_  @_  P_         �            �           �  �       ��$                           A   ����          ��               l`    �                               6   5        p   ��         D  l`    �                                       *                              �_   �_   �_   `   `    `                   �  �           ,`  <`  L`  \`           4`  D`  T`  d`         �            �   �        O   ����  e�          O   ����  R�          O   ����  ��      H  9   ;     a                     a                      a                     ,a                     8a                     Da                     Pa                     \a      	       	       �a                         � ߱            $  <  <  ���    	                         �      P  \2               ��                  H  Y  8              ,Nd                     H  t  �
  	  l	       ��                            7   ����          ��               @b    �            �	                  6   H        
   ��         �	  @b    �            �	                                                        �a   �a   �a   �a   �a   �a                   l
  `
            b  b   b  0b           b  b  (b  8b                      (
   D
        �
  �
       ��$                           A   ����          ��               Xc    �            L                  6   H        �   ��         p  Xc    �            L                          *                              �b   �b   �b    c   c                 �  �           c  (c  8c  Hc            c  0c  @c  Pc         �            �   �        O   ����  e�          O   ����  R�          O   ����  ��      l  9   N     �c                     �c                     �c                     d                     d                      d                     ,d                     8d      	       	       ld                         � ߱            $  O  `  ���    	                               t  L2      D  ,      ��                  [  l  \              ��b                �     [  �  �  @  �       ��                            7   ����          ��               e    �            �                  6   [        (   ��           e    �            �                                                        �d   �d   �d   �d   �d                 �  �           �d  �d  �d  �d  e           �d  �d  �d   e  e         �            D   d        �          ��$                           A   ����          ��               f    �            p                  6   [        �   ��         �  f    �            p                          *                              �e   �e   �e   �e   �e                              �e  �e  �e  f           �e  �e   f  f         �            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �  9   a     �f                     �f                     �f                     �f                     �f                     �f                     �f                     �f      	       	       ,g                         � ߱            $  b  �  ���    	                   t  A   o          ��                                                      Lg                 `  T           Xg           `g         �            4   D    8  $  p  �  ���                       hg                         � ߱              H         <2      �  �      ��                  s  �                ��b                <     s  �      t  �       ��                            7   ����          ��          	     �h    �                              6   s        \   ��        	 8  �h    �                                                                    h   h   $h   0h   <h                 �  �           Hh  Xh  hh  xh  �h           Ph  `h  ph  �h  �h         �            x   �        O   ����  e�          O   ����  R�          O   ����  ��        A  x        �   ��        
 p  �i                                         i   (i   4i   @i   Li                 �  �           Xi  hi  xi  �i           `i  pi  �i  �i         �            �   �    L    y  $  4      $j      4   ����$j      O   y  �� ��      �  $  {  x  ���                       0j                         � ߱        �  $  |  �  ���                       �j                         � ߱              ~    �      `k      4   ����`k                �                      ��                  ~  �                  �a                       ~  (          �  <      �k      4   �����k                L                      ��                  �  �                  ��a                       �  �  L  9   �     �k                     �k                     �k                     �k                     �k                     �k                     �k                     �k      	       	           � ߱        x  $  �  \  ���                           $  �  �  ���                       l                         � ߱              L                 �  �      ��                  �  �  �              83d                       �  �      x  �       ��                            7   ����           ��               �l    �                              6   �         T   ��         <  �l    �                                                                    <l   Hl   \l                 �  �           hl  xl           pl  �l                      p   �        O   ����  e�          O   ����  R�          O   ����  ��      X  $  �  ,  ���                       �l                         � ߱        �    �  t  �       m      4   ���� m  	                                     ��             	     �  �                  �a                       �  �  X  $  �  ,  ���                       xm                         � ߱              �  t  �      �m      4   �����m      O   �  �� ��      �  $  �  �  ���                       �m                         � ߱        �    �           �m      4   �����m      $  �  L  ���                       0n                         � ߱        
      �      \#  ,2      ,#  #      ��                  �  �  D#              �a                       �  x  �!      p        ��                            7   ����          ��               �n    �            �                   6   �        !   ��         �   �n    �            �                                                         pn   |n   �n   �n   �n                 p!  d!           �n  �n  �n  �n  �n           �n  �n  �n  �n  �n         �            $!   D!        �!   "       ��$                           A   ����          ��               p    �            P"                  6   �        �"   ��         t"  p    �            P"                          *                              �o   �o   �o   �o   �o   �o                    #  �"           �o  �o  �o  �o           �o  �o  �o   p         �            �"   �"        O   ����
 
 e�          O   ����
 
 R�          O   ����
 
 ��      �#  $  �  �#  ���                       �p                         � ߱        $  $  �  �#  ���                       q                         � ߱              �  ($  �$      �q      4   �����q                �$                      ��                  �  �                  �xa                       �  8$        �  �$  L%      r      4   ����r                \%                      ��                  �  �                  �xa                       �  �$  @&  9   �     0r                     <r                     Hr                     Tr                     `r                     lr                     xr      	       	           � ߱            $  �  l%  ���                             �&      t*          �)  �)      ��                  �  �  �)              tya                �/     �  l&  L(  '  d'       ��                            7   ����          ��               �r    �            �'                  6   �        �'   ��         �'  �r    �            �'                                                        �r   �r   �r                 8(  ,(           �r           �r                      (   (        x(  �(       ��                            7   ����    !      ��               �s    �            )                  6   �       ! T)   ��         <)  �s    �            )                                                        \s   hs   ts                 �)  �)           �s  �s  �s           �s  �s  �s         �            p)   �)        O   ����  e�          O   ����  R�          O   ����  ��            �*      �-  2      �-  �-      ��                  �  �  �-              �{a                       �  *  �+  �*   +       ��                            7   ����    "      ��               Pt    �            P+                  6   �       " �+   ��         t+  Pt    �            P+                                                        �s   t   t                 �+  �+            t  0t  @t           (t  8t  Ht                      �+   �+        $,  t,       ��                            7   ����    #      ��               Du    �            �,                  6   �       # -   ��         �,  Du    �            �,                                                        �t   �t   �t   �t   �t   �t                   �-  x-           �t  �t  u  u  $u  4u           �t  �t  u  u  ,u  <u                      0-   T-        O   ����  e�          O   ����  R�          O   ����  ��      $.    �  �-  .      v      4   ����v      O   �  �� ��      $/  9   �     Lv                     Xv                     �v                     �v                     �v                     �v                     w                     w      	       	           � ߱        P/  $  �  4.  ���                           $  �  |/  ���                       0w                         � ߱        �/  �   �  Pw      pw  �              � ߱            Z   �  �/  
 �                                     �0          l0  |0   @ <0                                                              0              0           ��                             ��                             ��                             ��                             ��                             ��                             ��                             ��                             ��                             ��                              ��        e                  ����                            |2        =   �         =   �         =   �         =   l         =   Y         =   F                           �           �   l       ��                      �               ��a                    O   ����    e�          O   ����    R�          O   ����    ��               �� �                   ��                              ��        e                  ����                                            �           �   l       ��                    &  �               ��c                    O   ����    e�          O   ����    R�          O   ����    ��      |w  �              � ߱        0  Z   !  �    �                            �               �              � ߱        \  h   #      �                            s   %  �                                 �         ��                            7   ����          	 ��                     �            T                  6   %         x  	 ��                    �            T                                                                �  �      	             	                @            �   �      ��                              ��        e                  ����                                    2                 �                    �           �   l       ��                  ,  :  �               �Ra                    O   ����    e�          O   ����    R�          O   ����    ��      �   /   4  �                                  3   �����w      /   6                                  3   �����w    ��                            ����                                @x               �t    `                      
 �                                                                 �  �     '         �                                      
 �                                                                �  �     ,  
       �                                     
 �                                                                �  �     2         �                                      
 �                                                                �  �     6         �                                      
 �                                                                �  �     E  
       �                                      
 �                                                                �  �     P  #       !                                    
 �                                                                �  �     V         $!                                      �                                                                                                                                                               d d     �   ��!  �!  � �       m  �                                  e   �                                                           
   d     D                                                                 H  d d @x                                           �           \  ���s                                 �                  -!                A      P   ,�JQ                                                           3!  G   
 X  ,��Q                                                             n      P �� �l2                                                        {       P �� l2                                                        �       P ����2                                                        �       P ���2                                             
           �        D                                                                        TXS appSrvUtils ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST S-CODCIA S-CODALM S-CODMAT tmp-tabla t-CodAlm t-CodDoc t-Nroped t-CodDiv t-FchPed t-NomCli t-CodMat t-Canped Btn_OK x-Total BROWSE-2 STOCK COMPROMETIDO x(3) x(10) XXX XXX-XXXXXXXXXX 99/99/9999 x(35) ->>,>>>,>>9.99 gDialog  ->>>>,>>9.99 O/D: Orden de Despacho R/A: Repos. Autom. OTR: Orden de Transferencia RAN: Repos. Autom. Nocturna DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDialog ContainerType DIALOG-BOX PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   BROWSE-2 Btn_OK CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target END-ERROR iStartPage A SmartDialog is not intended to be run  Persistent or to be placed in another  SmartObject at AppBuilder design time. ADM-CREATE-OBJECTS TimeOut TimeNow GENERAL Almacen I FacDPedi Detalle pedido credito PED P FacCPedi Pedidos al Credito G,X,P,W,WX,WL O/D WL,P OTR FacCfgGn Configuracion General P/M TimeLimit FacTabla Tablas Generales Facturacion GN-DIVI 0000 XX:XX HH:MM COT Almmmate almcrepo Reposiciones de mercaderia almdrepo A,M,RAN,INC RAN R/A 999 9999999 CARGA-TEMPORAL DISABLE_UI ENABLE_UI INITIALIZEOBJECT default Almac�n!Despacho Divisi�n Codigo!Documento Numero!Pedido   Fecha       !  Pedido        Cliente Cantidad SALIR TOTAL alm01 llave04 llave03 Llave01 mate01 Llave02 H  @  x  �%      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   L	  d	  f	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props W  X  Y  Z  \  ]  ^  _  `  a  b  c  f  i  j  k  l              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  �  T	  �	     =               �	                  adm-create-objects    �	        �	     TimeOut 
        
     TimeNow            
     TimeLimit   �	  l
  ?   >   �	          \
                  Carga-Temporal  .  1  2  5  ;  <  F  H  N  O  Y  [  a  b  l  o  p  s  x  y  {  |  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �     ,
  �     ?               �                  disable_UI      h  �     @               �                  enable_UI   !  #  %  &  �  <     A               (                  initializeObject    4  6  :  �    �      T                                 �  �     tmp-tabla   �                                     $         0         <         H         t-CodAlm    t-CodDoc    t-Nroped    t-CodDiv    t-FchPed    t-NomCli    t-CodMat    t-Canped    t          h  
   appSrvUtils �        �     S-CODCIA    �        �     S-CODALM    �        �     S-CODMAT    �       �     x-Total           
   gshAstraAppserver   @        ,  
   gshSessionManager   d  	 	     T  
   gshRIManager    �  
 
     x  
   gshSecurityManager  �        �  
   gshProfileManager   �        �  
   gshRepositoryManager            �  
   gshTranslationManager   0           
   gshWebManager   T        D     gscSessionId    x        h     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager           �     gsdTempUniqueID (             gsdUserObj  P        <     gsdRenderTypeObj    x        d     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf           �     glADMLoadFromRepos              glADMOk <       0  
   ghContainer \    	   P     cObjectName x    
   p     iStart  �       �     cAppService �       �     cASDivision �       �     cServerOperatingMode            �     cFields               iStartPage  <    L  0  tmp-tabla   T       L  Almacen p       d  FacDPedi    �       �  FacCPedi    �       �  FacCfgGn    �        �  FacTabla    �   !    �  Almmmate    �   "    �  almcrepo         #      almdrepo             ;   �  �  �  �  �  �  �          4  @  A  B  D  F  G  H  L  M  P  Q  R  S  U  W  Y  [  \  ]  `  b  c  e  f  g  h  i  o  q  w  y  {  |  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  
  
  
  
  
  
  
   
  !
  "
  #
  $
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
  e  q  �  �  �  �  �  �  �  �  �  �  �  �    9  ;  P  �  �  �             !  "  )  *  G  [  �          �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  W  ~  �  �  �  �  �  �  �  �  �  �        	  
        �� $ C:\Progress\OpenEdge\src\adm2\dialogmn.i   f!  C:\Progress\OpenEdge\src\adm2\containr.i 8  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    l  ��  C:\Progress\OpenEdge\src\adm2\visual.i   �  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    $  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   \  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �  Ds   C:\Progress\OpenEdge\gui\fn  �  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �  Q.  C:\Progress\OpenEdge\gui\set <  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i d  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �  F>  C:\Progress\OpenEdge\src\adm2\visprop.i     �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i T  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i �  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i      V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    L  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �  �j  C:\Progress\OpenEdge\gui\get �  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    0  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i t  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i     �  C:\Progress\OpenEdge\src\adm2\appsprto.i `  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  �  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i    n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i T  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i     �X    d:\newsie\on_in_co\APLIC\vtamay\c-conped - copia.w       �         �     �  $   �  �   �      �  �   �     �     v     �  �   q     �     O     �  �   G     �     �  #       �   �           �          �   �     0      �      @   �   �     P      �      `   r   �     p   n   �     �      >  "   �   i   9     �           �   P   �     �   �   �     �      �  !   �   �   �     �      v      !  �   u     !     S      !  �   Q     0!     /     @!  g        P!     �     `!  O   �     p!  �   h     �!     f      �!  �   6     �!     �     �!  �   �     �!     �     �!  �   �     �!     �     �!  �   �      "     k     "  �   j      "     H     0"  �   7     @"          P"  �        `"     �     p"  }   �     �"     �     �"     F     �"     �     �"     �     �"  7   n     �"  �   e     �"  O   W     �"     F      #     �
     #  �   �
      #  �   �
     0#  O   �
     @#     �
     P#     :
     `#  �   
     p#  x   
  
   �#  M   �	     �#     �	     �#     �	     �#  a   �	  
   �#  �  c	     �#     D	     �#  �  	     �#  O   	      $     �     $     �      $  �   �     0$     �     @$     �     P$  x   �     `$     �     p$     _     �$     [     �$     G     �$     .     �$  Q     
   �$     �     �$     �  
   �$     x     �$     ^  
    %  f   3     %     �  	    %  "   �     0%     z     @%     Y     P%  Z        `%          p%     �     �%     �     �%     �     �%     m     �%  +   �       �%     D      �%     !       �%           