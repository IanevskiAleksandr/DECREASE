/* ========================================================================
 * bootstrap-tour - v0.10.3
 * http://bootstraptour.com
 * ========================================================================
 * Copyright 2012-2015 Ulrich Sossou
 *
 * ========================================================================
 * Licensed under the MIT License (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://opensource.org/licenses/MIT
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * ========================================================================
 */
 var bind=function(f,h){return function(){return f.apply(h,arguments)}};
(function(f,h){return"function"===typeof define&&define.amd?define(["jquery"],function(c){return f.Tour=h(c)}):"object"===typeof exports?module.exports=h(require("jquery")):f.Tour=h(f.jQuery)})(window,function(f){var h=window.document;return function(){function c(a){this._showPopoverAndOverlay=bind(this._showPopoverAndOverlay,this);try{var b=window.localStorage}catch(e){b=!1}this._options=f.extend({name:"tour",steps:[],container:"body",autoscroll:!0,keyboard:!0,storage:b,debug:!1,backdrop:!1,backdropContainer:"body",
backdropPadding:0,redirect:!0,orphan:!1,duration:!1,delay:!1,basePath:"",template:'<div class="popover" role="tooltip"> <div class="arrow"></div> <h3 class="popover-title"></h3> <div class="popover-content"></div> <div class="popover-navigation"> <div class="btn-group"> <button class="btn btn-sm btn-default" data-role="prev">&laquo; Prev</button> <button class="btn btn-sm btn-default" data-role="next">Next &raquo;</button> <button class="btn btn-sm btn-default" data-role="pause-resume" data-pause-text="Pause" data-resume-text="Resume">Pause</button> </div> <button class="btn btn-sm btn-default" data-role="end">End tour</button> </div> </div>',
afterSetState:function(a,b){},afterGetState:function(a,b){},afterRemoveState:function(a){},onStart:function(a){},onEnd:function(a){},onShow:function(a){},onShown:function(a){},onHide:function(a){},onHidden:function(a){},onNext:function(a){},onPrev:function(a){},onPause:function(a,b){},onResume:function(a,b){},onRedirectError:function(a){}},a);this._inited=this._force=!1;this._current=null;this.backdrops=[];this}c.prototype.addSteps=function(a){var b;var e=0;for(b=a.length;e<b;e++){var c=a[e];this.addStep(c)}return this};
c.prototype.addStep=function(a){this._options.steps.push(a);return this};c.prototype.getStep=function(a){if(null!=this._options.steps[a])return f.extend({id:"step-"+a,path:"",host:"",placement:"right",title:"",content:"<p></p>",next:a===this._options.steps.length-1?-1:a+1,prev:a-1,animation:!0,container:this._options.container,autoscroll:this._options.autoscroll,backdrop:this._options.backdrop,backdropContainer:this._options.backdropContainer,backdropPadding:this._options.backdropPadding,redirect:this._options.redirect,
reflexElement:this._options.steps[a].element,backdropElement:this._options.steps[a].element,orphan:this._options.orphan,duration:this._options.duration,delay:this._options.delay,template:this._options.template,onShow:this._options.onShow,onShown:this._options.onShown,onHide:this._options.onHide,onHidden:this._options.onHidden,onNext:this._options.onNext,onPrev:this._options.onPrev,onPause:this._options.onPause,onResume:this._options.onResume,onRedirectError:this._options.onRedirectError},this._options.steps[a])};
c.prototype.init=function(a){this._force=a;if(this.ended())return this._debug("Tour ended, init prevented."),this;this.setCurrentStep();this._initMouseNavigation();this._initKeyboardNavigation();this._onResize(function(a){return function(){return a.showStep(a._current)}}(this));this._onScroll(function(a){return function(){return a._showPopoverAndOverlay(a._current)}}(this));null!==this._current&&this.showStep(this._current);this._inited=!0;return this};c.prototype.start=function(a){null==a&&(a=!1);
this._inited||this.init(a);null===this._current&&(a=this._makePromise(null!=this._options.onStart?this._options.onStart(this):void 0),this._callOnPromiseDone(a,this.showStep,0));return this};c.prototype.next=function(){var a=this.hideStep(this._current,this._current+1);return this._callOnPromiseDone(a,this._showNextStep)};c.prototype.prev=function(){var a=this.hideStep(this._current,this._current-1);return this._callOnPromiseDone(a,this._showPrevStep)};c.prototype.goTo=function(a){var b=this.hideStep(this._current,
a);return this._callOnPromiseDone(b,this.showStep,a)};c.prototype.end=function(){var a=function(a){return function(b){f(h).off("click.tour-"+a._options.name);f(h).off("keyup.tour-"+a._options.name);f(window).off("resize.tour-"+a._options.name);f(window).off("scroll.tour-"+a._options.name);a._setState("end","yes");a._inited=!1;a._force=!1;a._clearTimer();if(null!=a._options.onEnd)return a._options.onEnd(a)}}(this);var b=this.hideStep(this._current);return this._callOnPromiseDone(b,a)};c.prototype.ended=
function(){return!this._force&&!!this._getState("end")};c.prototype.restart=function(){this._removeState("current_step");this._removeState("end");this._removeState("redirect_to");return this.start()};c.prototype.pause=function(){var a=this.getStep(this._current);if(!a||!a.duration)return this;this._paused=!0;this._duration-=(new Date).getTime()-this._start;window.clearTimeout(this._timer);this._debug("Paused/Stopped step "+(this._current+1)+" timer ("+this._duration+" remaining).");if(null!=a.onPause)return a.onPause(this,
this._duration)};c.prototype.resume=function(){var a=this.getStep(this._current);if(!a||!a.duration)return this;this._paused=!1;this._start=(new Date).getTime();this._duration=this._duration||a.duration;this._timer=window.setTimeout(function(a){return function(){return a._isLast()?a.next():a.end()}}(this),this._duration);this._debug("Started step "+(this._current+1)+" timer with duration "+this._duration);if(null!=a.onResume&&this._duration!==a.duration)return a.onResume(this,this._duration)};c.prototype.hideStep=
function(a,b){var e;if(e=this.getStep(a)){this._clearTimer();var c=this._makePromise(null!=e.onHide?e.onHide(this,a):void 0);var d=function(c){return function(d){d=f(e.element);d.data("bs.popover")||d.data("popover")||(d=f("body"));d.popover("destroy").removeClass("tour-"+c._options.name+"-element tour-"+c._options.name+"-"+a+"-element").removeData("bs.popover");e.reflex&&f(e.reflexElement).removeClass("tour-step-element-reflex").off(c._reflexEvent(e.reflex)+".tour-"+c._options.name);e.backdrop&&
((d=null!=b&&c.getStep(b))&&d.backdrop&&d.backdropElement===e.backdropElement||c._hideOverlayElement(e));if(null!=e.onHidden)return e.onHidden(c)}}(this);var g=e.delay.hide||e.delay;"[object Number]"==={}.toString.call(g)&&0<g?(this._debug("Wait "+g+" milliseconds to hide the step "+(this._current+1)),window.setTimeout(function(a){return function(){return a._callOnPromiseDone(c,d)}}(this),g)):this._callOnPromiseDone(c,d);return c}};c.prototype.showStep=function(a){var b;if(this.ended())return this._debug("Tour ended, showStep prevented."),
this;if(b=this.getStep(a)){var e=a<this._current;var c=this._makePromise(null!=b.onShow?b.onShow(this,a):void 0);this.setCurrentStep(a);var d=function(){switch({}.toString.call(b.path)){case "[object Function]":return b.path();case "[object String]":return this._options.basePath+b.path;default:return b.path}}.call(this);if(b.redirect&&this._isRedirect(b.host,d,h.location)&&(this._redirect(b,a,d),!this._isJustPathHashDifferent(b.host,d,h.location)))return;var f=function(c){return function(d){if(c._isOrphan(b)){if(!1===
b.orphan){c._debug("Skip the orphan step "+(c._current+1)+".\nOrphan option is false and the element does not exist or is hidden.");e?c._showPrevStep():c._showNextStep();return}c._debug("Show the orphan step "+(c._current+1)+". Orphans option is true.")}b.autoscroll?c._scrollIntoView(a):c._showPopoverAndOverlay(a);if(b.duration)return c.resume()}}(this);d=b.delay.show||b.delay;"[object Number]"==={}.toString.call(d)&&0<d?(this._debug("Wait "+d+" milliseconds to show the step "+(this._current+1)),
window.setTimeout(function(a){return function(){return a._callOnPromiseDone(c,f)}}(this),d)):this._callOnPromiseDone(c,f);return c}};c.prototype.getCurrentStep=function(){return this._current};c.prototype.setCurrentStep=function(a){null!=a?(this._current=a,this._setState("current_step",a)):(this._current=this._getState("current_step"),this._current=null===this._current?null:parseInt(this._current,10));return this};c.prototype.redraw=function(){return this._showOverlayElement(this.getStep(this.getCurrentStep()))};
c.prototype._setState=function(a,b){if(this._options.storage){var e=this._options.name+"_"+a;try{this._options.storage.setItem(e,b)}catch(d){var c=d;c.code===DOMException.QUOTA_EXCEEDED_ERR&&this._debug("LocalStorage quota exceeded. State storage failed.")}return this._options.afterSetState(e,b)}null==this._state&&(this._state={});return this._state[a]=b};c.prototype._removeState=function(a){if(this._options.storage)return a=this._options.name+"_"+a,this._options.storage.removeItem(a),this._options.afterRemoveState(a);
if(null!=this._state)return delete this._state[a]};c.prototype._getState=function(a){if(this._options.storage){var b=this._options.name+"_"+a;b=this._options.storage.getItem(b)}else null!=this._state&&(b=this._state[a]);if(void 0===b||"null"===b)b=null;this._options.afterGetState(a,b);return b};c.prototype._showNextStep=function(){var a=this.getStep(this._current);var b=function(b){return function(e){return b.showStep(a.next)}}(this);var e=this._makePromise(null!=a.onNext?a.onNext(this):void 0);return this._callOnPromiseDone(e,
b)};c.prototype._showPrevStep=function(){var a=this.getStep(this._current);var b=function(b){return function(e){return b.showStep(a.prev)}}(this);var e=this._makePromise(null!=a.onPrev?a.onPrev(this):void 0);return this._callOnPromiseDone(e,b)};c.prototype._debug=function(a){if(this._options.debug)return window.console.log("Bootstrap Tour '"+this._options.name+"' | "+a)};c.prototype._isRedirect=function(a,b,e){if(null!=a&&""!==a&&("[object RegExp]"==={}.toString.call(a)&&!a.test(e.origin)||"[object String]"===
{}.toString.call(a)&&this._isHostDifferent(a,e)))return!0;a=[e.pathname,e.search,e.hash].join("");return null!=b&&""!==b&&("[object RegExp]"==={}.toString.call(b)&&!b.test(a)||"[object String]"==={}.toString.call(b)&&this._isPathDifferent(b,a))};c.prototype._isHostDifferent=function(a,b){switch({}.toString.call(a)){case "[object RegExp]":return!a.test(b.origin);case "[object String]":return this._getProtocol(a)!==this._getProtocol(b.href)||this._getHost(a)!==this._getHost(b.href);default:return!0}};
c.prototype._isPathDifferent=function(a,b){return this._getPath(a)!==this._getPath(b)||!this._equal(this._getQuery(a),this._getQuery(b))||!this._equal(this._getHash(a),this._getHash(b))};c.prototype._isJustPathHashDifferent=function(a,b,e){if(null!=a&&""!==a&&this._isHostDifferent(a,e))return!1;a=[e.pathname,e.search,e.hash].join("");return"[object String]"==={}.toString.call(b)?this._getPath(b)===this._getPath(a)&&this._equal(this._getQuery(b),this._getQuery(a))&&!this._equal(this._getHash(b),this._getHash(a)):
!1};c.prototype._redirect=function(a,b,e){if(f.isFunction(a.redirect))return a.redirect.call(this,e);var c="[object String]"==={}.toString.call(a.host)?""+a.host+e:e;this._debug("Redirect to "+c);if(this._getState("redirect_to")===""+b){if(this._debug("Error redirection loop to "+e),this._removeState("redirect_to"),null!=a.onRedirectError)return a.onRedirectError(this)}else return this._setState("redirect_to",""+b),h.location.href=c};c.prototype._isOrphan=function(a){return null==a.element||!f(a.element).length||
f(a.element).is(":hidden")&&"http://www.w3.org/2000/svg"!==f(a.element)[0].namespaceURI};c.prototype._isLast=function(){return this._current<this._options.steps.length-1};c.prototype._showPopoverAndOverlay=function(a){if(this.getCurrentStep()===a&&!this.ended()){var b=this.getStep(a);b.backdrop&&this._showOverlayElement(b);this._showPopover(b,a);if(null!=b.onShown)b.onShown(this);return this._debug("Step "+(this._current+1)+" of "+this._options.steps.length)}};c.prototype._showPopover=function(a,
b){f(".tour-"+this._options.name).remove();var e=f.extend({},this._options);var c=this._isOrphan(a);a.template=this._template(a,b);c&&(a.element="body",a.placement="top");var d=f(a.element);d.addClass("tour-"+this._options.name+"-element tour-"+this._options.name+"-"+b+"-element");a.options&&f.extend(e,a.options);if(a.reflex&&!c)f(a.reflexElement).addClass("tour-step-element-reflex").off(this._reflexEvent(a.reflex)+".tour-"+this._options.name).on(this._reflexEvent(a.reflex)+".tour-"+this._options.name,
function(a){return function(){return a._isLast()?a.next():a.end()}}(this));e=!0===a.smartPlacement&&-1===a.placement.search(/auto/i);d.popover({placement:e?"auto "+a.placement:a.placement,trigger:"manual",title:a.title,content:a.content,html:!0,animation:a.animation,container:a.container,template:a.template,selector:a.element}).popover("show");e=d.data("bs.popover")?d.data("bs.popover").tip():d.data("popover").tip();e.attr("id",a.id);"fixed"===d.css("position")&&e.css("position","fixed");this._reposition(e,
a);if(c)return this._center(e)};c.prototype._template=function(a,b){var e=a.template;this._isOrphan(a)&&"[object Boolean]"!=={}.toString.call(a.orphan)&&(e=a.orphan);var c=f.isFunction(e)?f(e(b,a)):f(e);var d=c.find(".popover-navigation");var g=d.find('[data-role="prev"]');e=d.find('[data-role="next"]');d=d.find('[data-role="pause-resume"]');this._isOrphan(a)&&c.addClass("orphan");c.addClass("tour-"+this._options.name+" tour-"+this._options.name+"-"+b);a.reflex&&c.addClass("tour-"+this._options.name+
"-reflex");0>a.prev&&g.addClass("disabled").prop("disabled",!0).prop("tabindex",-1);0>a.next&&e.addClass("disabled").prop("disabled",!0).prop("tabindex",-1);a.duration||d.remove();return c.clone().wrap("<div>").parent().html()};c.prototype._reflexEvent=function(a){return"[object Boolean]"==={}.toString.call(a)?"click":a};c.prototype._reposition=function(a,b){var c=a[0].offsetWidth;var l=a[0].offsetHeight;var d=a.offset();var g=d.left;var m=d.top;var k=f(h).outerHeight()-d.top-a.outerHeight();0>k&&
(d.top+=k);k=f("html").outerWidth()-d.left-a.outerWidth();0>k&&(d.left+=k);0>d.top&&(d.top=0);0>d.left&&(d.left=0);a.offset(d);if("bottom"===b.placement||"top"===b.placement){if(g!==d.left)return this._replaceArrow(a,2*(d.left-g),c,"left")}else if(m!==d.top)return this._replaceArrow(a,2*(d.top-m),l,"top")};c.prototype._center=function(a){return a.css("top",f(window).outerHeight()/2-a.outerHeight()/2)};c.prototype._replaceArrow=function(a,b,c,f){return a.find(".arrow").css(f,b?50*(1-b/c)+"%":"")};
c.prototype._scrollIntoView=function(a){var b=this.getStep(a);var c=f(b.element);if(!c.length)return this._showPopoverAndOverlay(a);var l=f(window);var d=c.offset().top;var g=c.outerHeight();var h=l.height();var k=0;switch(b.placement){case "top":k=Math.max(0,d-h/2);break;case "left":case "right":k=Math.max(0,d+g/2-h/2);break;case "bottom":k=Math.max(0,d+g-h/2)}this._debug("Scroll into view. ScrollTop: "+k+". Element offset: "+d+". Window height: "+h+".");var n=0;return f("body, html").stop(!0,!0).animate({scrollTop:Math.ceil(k)},
function(b){return function(){if(2===++n)return b._showPopoverAndOverlay(a),b._debug("Scroll into view.\nAnimation end element offset: "+c.offset().top+".\nWindow height: "+l.height()+".")}}(this))};c.prototype._onResize=function(a,b){return f(window).on("resize.tour-"+this._options.name,function(){clearTimeout(b);return b=setTimeout(a,100)})};c.prototype._onScroll=function(a,b){return f(window).on("scroll.tour-"+this._options.name,function(){clearTimeout(b);return b=setTimeout(a,100)})};c.prototype._initMouseNavigation=
function(){var a=this;return f(h).off("click.tour-"+this._options.name,".popover.tour-"+this._options.name+" *[data-role='prev']").off("click.tour-"+this._options.name,".popover.tour-"+this._options.name+" *[data-role='next']").off("click.tour-"+this._options.name,".popover.tour-"+this._options.name+" *[data-role='end']").off("click.tour-"+this._options.name,".popover.tour-"+this._options.name+" *[data-role='pause-resume']").on("click.tour-"+this._options.name,".popover.tour-"+this._options.name+
" *[data-role='next']",function(a){return function(b){b.preventDefault();return a.next()}}(this)).on("click.tour-"+this._options.name,".popover.tour-"+this._options.name+" *[data-role='prev']",function(a){return function(b){b.preventDefault();if(0<a._current)return a.prev()}}(this)).on("click.tour-"+this._options.name,".popover.tour-"+this._options.name+" *[data-role='end']",function(a){return function(b){b.preventDefault();return a.end()}}(this)).on("click.tour-"+this._options.name,".popover.tour-"+
this._options.name+" *[data-role='pause-resume']",function(b){b.preventDefault();b=f(this);b.text(a._paused?b.data("pause-text"):b.data("resume-text"));return a._paused?a.resume():a.pause()})};c.prototype._initKeyboardNavigation=function(){if(this._options.keyboard)return f(h).on("keyup.tour-"+this._options.name,function(a){return function(b){if(b.which)switch(b.which){case 39:return b.preventDefault(),a._isLast()?a.next():a.end();case 37:if(b.preventDefault(),0<a._current)return a.prev()}}}(this))};
c.prototype._makePromise=function(a){return a&&f.isFunction(a.then)?a:null};c.prototype._callOnPromiseDone=function(a,b,c){return a?a.then(function(a){return function(e){return b.call(a,c)}}(this)):b.call(this,c)};c.prototype._showBackground=function(a,b){var c,l;var d=f(h).height();var g=f(h).width();var m=["top","bottom","left","right"];var k=[];var n=0;for(l=m.length;n<l;n++){var p=m[n];var q=null!=(c=this.backdrops)[p]?c[p]:c[p]=f("<div>",{"class":"tour-backdrop "+p});f(a.backdropContainer).append(q);
switch(p){case "top":k.push(q.height(0<b.offset.top?b.offset.top:0).width(g).offset({top:0,left:0}));break;case "bottom":k.push(q.offset({top:b.offset.top+b.height,left:0}).height(d-(b.offset.top+b.height)).width(g));break;case "left":k.push(q.offset({top:b.offset.top,left:0}).height(b.height).width(0<b.offset.left?b.offset.left:0));break;case "right":k.push(q.offset({top:b.offset.top,left:b.offset.left+b.width}).height(b.height).width(g-(b.offset.left+b.width)));break;default:k.push(void 0)}}return k};
c.prototype._showOverlayElement=function(a){var b=f(a.backdropElement);if(0===b.length)var c={width:0,height:0,offset:{top:0,left:0}};else c={width:b.innerWidth(),height:b.innerHeight(),offset:b.offset()},b.addClass("tour-step-backdrop"),a.backdropPadding&&(c=this._applyBackdropPadding(a.backdropPadding,c));return this._showBackground(a,c)};c.prototype._hideOverlayElement=function(a){var b;f(a.backdropElement).removeClass("tour-step-backdrop");var c=this.backdrops;for(b in c)(a=c[b])&&void 0!==a.remove&&
a.remove();return this.backdrops=[]};c.prototype._applyBackdropPadding=function(a,b){"object"===typeof a?(null==a.top&&(a.top=0),null==a.right&&(a.right=0),null==a.bottom&&(a.bottom=0),null==a.left&&(a.left=0),b.offset.top-=a.top,b.offset.left-=a.left,b.width=b.width+a.left+a.right,b.height=b.height+a.top+a.bottom):(b.offset.top-=a,b.offset.left-=a,b.width+=2*a,b.height+=2*a);return b};c.prototype._clearTimer=function(){window.clearTimeout(this._timer);return this._duration=this._timer=null};c.prototype._getProtocol=
function(a){a=a.split("://");return 1<a.length?a[0]:"http"};c.prototype._getHost=function(a){a=a.split("//");a=1<a.length?a[1]:a[0];return a.split("/")[0]};c.prototype._getPath=function(a){return a.replace(/\/?$/,"").split("?")[0].split("#")[0]};c.prototype._getQuery=function(a){return this._getParams(a,"?")};c.prototype._getHash=function(a){return this._getParams(a,"#")};c.prototype._getParams=function(a,b){var c;var f=a.split(b);if(1===f.length)return{};f=f[1].split("&");var d={};var g=0;for(c=
f.length;g<c;g++){var h=f[g];h=h.split("=");d[h[0]]=h[1]||""}return d};c.prototype._equal=function(a,b){if("[object Object]"==={}.toString.call(a)&&"[object Object]"==={}.toString.call(b)){var c=Object.keys(a);var f=Object.keys(b);if(c.length!==f.length)return!1;for(g in a){var d=a[g];if(!this._equal(b[g],d))return!1}return!0}if("[object Array]"==={}.toString.call(a)&&"[object Array]"==={}.toString.call(b)){if(a.length!==b.length)return!1;var g=c=0;for(f=a.length;c<f;g=++c)if(d=a[g],!this._equal(d,
b[g]))return!1;return!0}return a===b};return c}()});
 var tourused = 0;
 


var tourused = 0;

var tour = new Tour({
	  storage: false,
	  orphan: true,
	  smartPlacement: true,
	  autoscroll: false,
	  backdrop: true,
	  onEnd: function (tour) { window.location.href = '//decrease.fimm.fi';    }
  })

$( document ).ready(function() {

    tour.addSteps([
      {
      element: "#getstarted2",
      title: "Welcome!", 
      content: "Welcome to DECREASE exploration tour. <br>It will indroduce you to our application by walking through it step by step.",
	  placement: "right",
	  duration: 6000
	  },
	  { 
		element: "#getstarted",
		title: "Get started!",
		content: "Let's get started by clicking<br>'Get Started' button",
		placement: "left",
		onNext: function (tour) { var aaa = Math.round(new Date().getTime()/1e3); Shiny.onInputChange("getstarted", { aaa }); },
		duration: 5000
	  },
	  {
		element: "#nonexisting",
		title: "Data preparation!", 
		content: "First step is to prepare your input file...",
		duration: 4000
	  },
	  {
		element: "#tourfiletype",
		title: "File formats!", 
		content: "For convenient use, DECREASE accepts two input file formats: <b>Tabular</b> and <b>Matrix</b>.<br>For more information please read a <a> section x </a> of technical documentation.",
		placement: "bottom",
		duration: 6000
	  },
	  {
		element: "#tourreadout",
		title: "Readout!",
		content: "Please also specify a readout,<br> which indicates whether percentages of cell proliferation inhibition or viability are provided as a response.",
		placement: "bottom",
		duration: 5000
	  },
	  {
		element: "#tourexpdata",
		title: "Example data!",
		content: "You can find example data for both readouts (Tabular and Matrix) here.",
		placement: "bottom",
		duration: 5000
	  },
  
  {
    element: "#tourinpfile",
    title: "Upload your input file!",
    content: "Now, when the file is uploaded, let's get started with analysis...",
	onShown: function (tour) { var aaa = Math.round(new Date().getTime()/1e3); Shiny.onInputChange("inpfilepreloadtour", { aaa }); },
	onNext: function (tour) { var aaa = Math.round(new Date().getTime()/1e3); Shiny.onInputChange("start", { aaa }); },
	duration: 5000
  },
  
  {
    element: "#nonexisting",
    title: "Uploaded combination!",
    content: "Here we can see one of our uploaded dose-response matrices",
	backdrop: false,
	duration: 10000
	//onShown: function (tour) { var aaa = Math.round(new Date().getTime()/1e3); Shiny.onInputChange("selInhViatour", { aaa }); },
	//onNext: function (tour) { var aaa = Math.round(new Date().getTime()/1e3); Shiny.onInputChange("methodstour", { aaa }); }
  },
  
  {
    element: ".selectWrap",
    title: "Other combinations!",
    content: "Any of other uploaded combinations can be selected here",
	placement: "top",
	duration: 7000,
	onNext: function (tour) {  $('#startanalysis').attr("disabled", true); }
  },
  
  {
    element: "#startanalysis",
    title: "Prediction!",
    content: "To predict the currently selected combination, please click:",
	placement: "left",
	duration: 7000,
	onNext: function (tour) {  $('#startanalysis').attr("disabled", false);  $('#startanalysisall').attr("disabled", true);  }
  },
  
  {
    element: "#startanalysisall",
    title: "Prediction!",
    content: "To predict all combinations at once, please click:",
	placement: "left",
	duration: 7000,
	onNext: function (tour) {  $('#startanalysisall').attr("disabled", false); var aaa = Math.round(new Date().getTime()/1e3); 
								$(".toHide4").hide(); $(".toHide3").show(); Shiny.onInputChange("showcomboplot", { aaa }); },
	onPrev: function (tour) { $('#startanalysisall').attr("disabled", false); $('#startanalysis').attr("disabled", true); }
  },
  
  {
    element: "#save_results",
    title: "Export results!",
    content: "Once the analysis is ready it is possible to export the result as <b>*.pdf</b> or <b>text</b> file.",
	backdrop: false,
	duration: 15000,
	onPrev: function (tour) { $('#startanalysisall').attr("disabled", true);   }
  },
  {
    element: "#noelementhaha",
    title: "",
    content: "Thank you!"
  }

 ]);

 // Initialize the tour
 tour.init();
 
 document.getElementById("getstarted2").addEventListener("click",function(){0==tourused?tour.start():tour.restart(),tourused++},!1);
})