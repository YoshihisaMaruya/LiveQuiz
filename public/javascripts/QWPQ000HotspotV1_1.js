/**
 * Project:     Hotspot: Question Writer Plugin Question Type
 * File:        QWPQ000HotspotV1_1.js
 *
 * Copyright 2012 Question Writer Corporation
 * 
 * This file is provided as an extension of the 
 * Question Writer desktop software. Where you hold a valid, paid-for
 * license for the Question Writer desktop software, you are hereby 
 * granted a license to use and modify this file for personal use
 * and use internal to your organization. All other rights are reserved. 
 * 
 * This application is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * @link http://www.questionwriter.com/
 * @copyright 2012 Question Writer Corporation
 * @author Alexander McCabe 
 * @package Cycle
 * @version 1.1
 * 
 * 1.0
 * 1.1 Move Scaling factor to main library, changed to search for 'webkit'
 */



//var cycleCSS = ".cycleresponselabel{cursor: pointer;cursor: hand;}";
//QWUtils.addCss(cycleCSS);


function QWPQ000HotspotV1_1(assessmentReference, theResponseID, parameterPairs) {
    //init
    this.assessmentRef = assessmentReference;
    this.respID = theResponseID;
    this.theDiv = this.assessmentRef.getNodeByID(this.respID);
   
	this.userAttemptMade = false;	
	//Green Square 100x100
	this.imagedata = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGQAAABkCAYAAABw4pVUAAABqElEQVR4nO3dIU4DQQBG4X8mHABZBzcoLUGD7DG4CeEmHGVxNWyhrg6J5ADtLqYlwdWUeYH3uUlG/MnL6Ck56GfTDONVkqTUy+j0xuE9SVLLa+artyQpSZJ+dpux3LXapSRl7DJfPZf0s4uM5b71HiUp41PNrixa79Derixqaiatd2ivZlJbb9BPBoExCIxBYAwCc3b0zev+8YQ7/r6X+cMx13whMAaBMQiMQWAMAmMQGIPAGATGIDAGgTEIjEFgDAJjEBiDwBgExiAwBoExCIxBYAwCYxAYg8AYBMYgMAaBMQiMQWAMAmMQGIPAGATGIDAGgTEIjEFgDAJjEBiDwBgExiAwBoExCIxBYAwCYxAYg8AYBMYgMAaBMQiMQWAMAmMQGIPAGATGIDAGgTEIjEFgDAJjEBiDwBgExiAwBoExCIxBYAwCYxAYg8AYBMYgMAaBMQiMQWAMAmMQGIPAlGP/+dbv8IXAGATGIDAGgTEITM2Qj9YjtDfkoybbrvUOHWy7mpv1Jsmy9RRlmZv1pnwf+9k0w3iXUs8bjvp/xuEztXSZr96S5Au/6jC2rWH+CgAAAABJRU5ErkJggg==";
	//Blue Circle 100x100
	this.answerIndicatorImageData="data:image/png;base64,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";
	
	//Grab values from Parameter Array
    var paramsArray = parameterPairs;
    this.answer=paramsArray["_answer"];
    this.coords=this.answer.split(",");
	this.answerWidth=this.coords[2]-this.coords[0];
	this.answerHeight=this.coords[3]-this.coords[1];
	
	
    this.maxScore=paramsArray["_maxScore"];

    if (paramsArray["_isFeedback"] == "true") {
        this.isFeedback = true;
        this.originalQuestionID = paramsArray["_originalquestionid"];
    } else {
        this.isFeedback = false;
    }
    
    this.answerIndicatorSize=paramsArray["_indicatorsize"];
    
    if( this.answerIndicatorSize==null){
	this.answerIndicatorSize=100;
	}
    
    this.firstOption=this.theDiv.childNodes[0];
    //alert(firstOption.width);
    
    if (!this.isFeedback) {
	var that = this;
	var func = function () { 
		//that.clicked();
		var waitForURLUpdate = function () {
			//1. Get the co-ordinates of the mouse click, relative to the non-zoomed image
			var locationElements=document.location.href.replaceAll(",","?").split("?");
			that.answerLeft=locationElements[locationElements.length-2];
			that.answerTop=locationElements[locationElements.length-1];
			
			//Webkit (Chrome/Safari) has a bug - if the image has been scaled, it is reporting a scaled value. Need to correct for that. 
			//Will place correct value in main library to make easy to correct
			if(that.assessmentRef.getAPIQWObject().scaleForImageMapCorrection){
				that.answerLeft/=that.assessmentRef.getAPIQWObject().scaleForImageMapCorrection();	
				that.answerTop/=that.assessmentRef.getAPIQWObject().scaleForImageMapCorrection();
			}
			//2. Place the indicator image on top of the image, offset by 50% width/height 
			that.replaceAnswerIndicator();
			that.updateAnswers(true);
		}
		//It takes a little time to upade the url with the x,y co-ordinates
		var t=setTimeout(waitForURLUpdate,100);
		
	};
	    
	if (this.firstOption.addEventListener) {
		this.firstOption.addEventListener('click', func, false);
	} else {
		this.firstOption.attachEvent('onclick', func);
	}
	this.theDiv.childNodes[0].childNodes[0].setAttribute("ismap","true");
	this.initialHTML=this.firstOption.innerHTML="<a href='#'>"+this.firstOption.innerHTML+"</a>";
 
	}else{
		//Draw / Place an image box around the correct answer location
		this.firstOption.innerHTML+="<img style='position: absolute; top:"+this.coords[1]+"px; left:"+this.coords[0]+"px' width='"+this.answerWidth+"' height='"+this.answerHeight+"' border='0' src='"+this.imagedata+"'>";
		this.initialHTML=this.firstOption.innerHTML;
		
	}
	
	//If answer is present, fetch it and put it in place
	this.putAnswersInPlace();
    
    
}


QWPQ000HotspotV1_1.prototype.updateAnswers = function (triggerFullyAnswered) {
	//Get array of answers
	var answers = new Array();
	//var optionID = this.theDiv.childNodes[this.currentOption].id.replace(this.respID, "");
	answers.push(this.answerLeft);
	answers.push(this.answerTop);
	this.assessmentRef.setAnswers(this.respID, answers, triggerFullyAnswered, this.getScore());
}

QWPQ000HotspotV1_1.prototype.replaceAnswerIndicator = function(){
	if(this.answerLeft==null ||this.answerLeft==""){
		this.userAttemptMade=false;
		return;
	}else{
		this.firstOption.innerHTML=this.initialHTML+"<img style='position: absolute; top:"+(this.answerTop-this.answerIndicatorSize/2)+"px; left:"+(this.answerLeft-this.answerIndicatorSize/2)+"px' width='"+this.answerIndicatorSize+"' height='"+this.answerIndicatorSize+"' border='0' src='"+this.answerIndicatorImageData+"'>";
		this.userAttemptMade=true;
	}
}

QWPQ000HotspotV1_1.prototype.isFullyAnswered = function () {
	return this.userAttemptMade;
}

QWPQ000HotspotV1_1.prototype.getScore = function () {
    if (this.answerLeft>this.coords[0] && this.answerLeft<this.coords[2] && this.answerTop>this.coords[1] && this.answerTop<this.coords[3]) {
        return Number(this.maxScore);
    } else {
        return 0;
    }
}


QWPQ000HotspotV1_1.prototype.putAnswersInPlace = function () {
    var answers;
    if (this.isFeedback) {
        answers = this.assessmentRef.getRespListAsArray(this.originalQuestionID);
    } else {
        answers = this.assessmentRef.getRespListAsArray(this.respID);
    }
	this.answerLeft=answers[0];
    	this.answerTop=answers[1];
	this.replaceAnswerIndicator();
}


