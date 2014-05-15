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
" tag
syn keyword     spaHtml5tag         !doctype a abbr address area article aside audio b base bdi bdo blockquote body br button canvas caption cite code col colgroup data datalist dd del details dfn dialog div dl dt em embed fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6 head header hr html i iframe img input ins kbd keygen label legend li link main map mark meta meter nav noscript object ol optgroup option output p param pre progress q rb rp rt ruby s samp script section select small source span strong style sub summary sup table tbody td template textarea tfoot th thead time title tr track u ul var video wbr
hi def link     spaHtml5tag         Identifier

" attribute
syn match       spaHtmlKey          /=\zs[^ $]*\ze/
hi def link     spaHtmlKey          Type
syn region      spaHtmlValue        start=/"/ skip=/\\"/ end=/"/
hi def link     spaHtmlValue        Type

" id / class
syn match       spaHtmlId           /#[^ ]*/
hi def link     spaHtmlId           Special
syn match       spaHtmlClass        /\.[^ ]*/
hi def link     spaHtmlClass        Type

" --- css
syn keyword     spaCSS3Property     @charset @font-face @import @keyframes @media @namespace @page @page:first @page:left @page:right animation animation-delay animation-direction animation-duration animation-fill-mode animation-iteration-count animation-name animation-play-state animation-timing-function appearance backface-visibility background background-attachment background-clip background-color background-image background-origin background-position background-repeat background-size border border-collapse border-color border-image border-radius border-spacing border-style border-top border-top-color border-top-left-radius border-top-style border-top-width border-width bottom box-align box-decoration-break box-flex box-ordinal-group box-orient box-pack box-shadow box-sizing break-after break-before break-inside caption-side clear clip color column-count column-fill column-gap column-rule column-rule-color column-rule-style column-rule-width columns column-span column-width content counter-increment counter-reset cursor direction display display display empty-cells filter float flow-from flow-into font font-family font-feature-settings font-size font-size-adjust font-style font-variant font-weight grid-column grid-column-align grid-columns grid-column-span grid-row grid-row-align grid-rows grid-row-span height hyphens image-rendering left letter-spacing line-break line-height list-style list-style-image list-style-position list-style-type margin margin-top marquee-direction marquee-play-count marquee-speed marquee-style max-height max-width min-height min-width object-fit object-position opacity orphans outline outline-color outline-offset outline-style outline-width overflow overflow-x overflow-y padding padding-top page-break-after page-break-before page-break-inside perspective perspective-origin position position quotes resize right ruby-align ruby-position table-layout tab-size text-align text-align-last text-autospace text-decoration text-decoration-line text-decoration-color text-decoration-style text-emphasis text-emphasis-color text-emphasis-position text-emphasis-style text-indent text-justify text-overflow text-shadow text-transform text-underline-position top transform transform transform-origin transform-origin transform-style transition transition-delay transition-duration transition-property transition-timing-function unicode-bidi vertical-align visibility white-space widows width word-break word-spacing word-wrap wrap-flow wrap-margin writing-mode z-index  
hi def link     spaCSS3Property     Identifier

syn match       spaCSSValue         /[0-9]*px/
syn match       spaCSSValue         /[0-9]*em/
syn keyword     spaCSSValue         none hidden solid double groove ridge inset outset dashed dotted bold
hi def link     spaCSSValue         Type

" --- javascript
" TODO

" --- programs
" variable
syn match       spaVariable         /$[^ ]*/
hi def link     spaVariable         String
syn match       spaFixed            /@[^ ]*/
hi def link     spaFixed            String
syn match       spaControlStructure /%.*/
hi def link     spaControlStructure Statement

let b:current_syntax = "spa"
