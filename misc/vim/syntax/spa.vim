" :h group-name
if exists("b:current_syntax")
  finish
endif

setlocal isk+=!
setlocal isk+=:
setlocal isk+=@
setlocal isk+=-
syn case ignore
syn sync minlines=500

" --- html
" tag exclude style and script
syn keyword     spaHtml5Tag         !doctype a abbr address area article aside audio b base bdi bdo blockquote body br button canvas caption cite code col colgroup data datalist dd del details dfn dialog div dl dt em embed fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6 head header hr html i iframe img input ins kbd keygen label legend li link main map mark meta meter nav noscript object ol optgroup option output p param pre progress q rb rp rt ruby s samp section select small source span strong sub summary sup table tbody td template textarea tfoot th thead time title tr track u ul var video wbr
hi def link     spaHtml5Tag         Keyword

" attribute
syn match       spaAttribute        / [^ ]*=\("[^"]*"\|'[^']*'\|[^ ]*\)/ contains=spaAttributeKey,spaAttributeValue,spaAttributeSplit
syn match       spaAttributeKey     / [^ =]*\(=\)\@=/ contained
hi def link     spaAttributeKey     Type
syn match       spaAttributeValue   /\(=\)\@<=\("[^"]*"\|'[^']*'\|[^ ]*\)*/ contained
hi def link     spaAttributeValue   String
syn match       spaAttributeSplit   /=/ contained
hi def link     spaAttributeSplit   Identifier

" id / class
syn match       spaHtmlId           /#[^ ]*/
hi def link     spaHtmlId           Keyword
syn match       spaHtmlClass        /\.[^ ]*/
hi def link     spaHtmlClass        Keyword

" macro
syn match       spaPlainText        /:[^ ]*/
hi def link     spaPlainText        Macro

" --- css
syn match       spaCssTag           /style /
hi def link     spaCssTag           Function
syn match       spaCss              /^ *style\n\( *[^ ].*\n\)*/ contains=spaCssKey,spaCssValue
hi def link     spaCss              Function
syn keyword     spaCssKey           @charset @font-face @import @keyframes @media @namespace @page @page:first @page:left @page:right animation animation-delay animation-direction animation-duration animation-fill-mode animation-iteration-count animation-name animation-play-state animation-timing-function appearance backface-visibility background background-attachment background-clip background-color background-image background-origin background-position background-repeat background-size border border-collapse border-color border-image border-radius border-spacing border-style border-top border-top-color border-top-left-radius border-top-style border-top-width border-width bottom box-align box-decoration-break box-flex box-ordinal-group box-orient box-pack box-shadow box-sizing break-after break-before break-inside caption-side clear clip color column-count column-fill column-gap column-rule column-rule-color column-rule-style column-rule-width columns column-span column-width content counter-increment counter-reset cursor direction display display display empty-cells filter float flow-from flow-into font font-family font-feature-settings font-size font-size-adjust font-style font-variant font-weight grid-column grid-column-align grid-columns grid-column-span grid-row grid-row-align grid-rows grid-row-span height hyphens image-rendering left letter-spacing line-break line-height list-style list-style-image list-style-position list-style-type margin margin-top marquee-direction marquee-play-count marquee-speed marquee-style max-height max-width min-height min-width object-fit object-position opacity orphans outline outline-color outline-offset outline-style outline-width overflow overflow-x overflow-y padding padding-top page-break-after page-break-before page-break-inside perspective perspective-origin position position quotes resize right ruby-align ruby-position table-layout tab-size text-align text-align-last text-autospace text-decoration text-decoration-line text-decoration-color text-decoration-style text-emphasis text-emphasis-color text-emphasis-position text-emphasis-style text-indent text-justify text-overflow text-shadow text-transform text-underline-position top transform transform transform-origin transform-origin transform-style transition transition-delay transition-duration transition-property transition-timing-function unicode-bidi vertical-align visibility white-space widows width word-break word-spacing word-wrap wrap-flow wrap-margin writing-mode z-index contained
hi def link     spaCssKey           Function
syn keyword     spaCssValue         left right top bottom none hidden solid double groove ridge inset outset dashed dotted bold contained
syn match       spaCssValue         /[0-9]*\(px\|em\)/ contained
hi def link     spaCssValue         Number

" --- javascript
syn match       spaJsTag            /script /
hi def link     spaJsTag            Function
syn match       spaJs               /^* script\n\( *[^ ].*\n\)*/ contains=spaJsEvent
hi def link     spaJs               Function
syn match       spaJsEvent          /on[^ ]*/ contained
hi def link     spaJsEvent          Keyword

" --- programs
" variable
syn match       spaVariable         /$[^ ]*/
hi def link     spaVariable         Identifier
syn region      spaVariable         start="${" end="}"
hi def link     spaVariable         Identifier

" expression
syn region      spaVariable         start="{" end="}" skip=+\\"}+
hi def link     spaVariable         Identifier

" statement
syn match       spaStatement        /\(if\|else\|for\).*$/ contains=spaConditional,spaRepeat,spaStatementLine
hi def link     spaStatement        Identifier

syn keyword     spaConditional      if else contained
hi def link     spaConditional      Conditional

syn keyword     spaRepeat           for contained
hi def link     spaRepeat           Repeat

syn match       spaStatementLine    /if\zs.*$/
hi def link     spaStatementLine    Type

" comment
syn match       spaComment          /^--.*/
hi def link     spaComment          Comment

let b:current_syntax = "spa"
