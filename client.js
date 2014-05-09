/**
 * @jsx React.DOM
 */

Date.prototype.modifiedJulianDay = function() {
  return Math.floor((this/86400000)-(this.getTimezoneOffset()/1440)+40587) }

var normalizeNote = function(note) {
  return note.replace(/\s+/g,' ').trim() }

var tok = localStorage.getItem("token")
var user = localStorage.getItem("username")
var julian = function(d) { return d.modifiedJulianDay() }
var epoch = julian(new Date(0))
var fromJulian = function(j) { return new Date(86400000*(j-epoch)) }
var day = julian(new Date())
var shiftDate = function(n,cont){ return(function(){ day+=n; cont() })}
var member = function(i,a) { return !(-1 === a.indexOf(i)) }

onEnter = function(j,f) {
  j.on("keyup",function(e){
    if (e.keyCode == 13)
      f() })
  return j }

var noteDOM = function(name) {
  return ($("<li>")
    .append($("<input>")
      .val(name)
      .addClass("note")
      .on("keyup",function(e){ if (e.keyCode==13) this.blur(); })
      .change(function(){
        this.blur()
        var a = normalizeNote(name);
        var b = normalizeNote(this.value)
        if (a == b) return
        delNote(name,addNote(b,update))() }))
    .addClass("note")) }

var habitDOM = function(name, status, tail, callback) {
  var result = $("<span>")
  result.addClass("refutable")
  result.text(name)
  result.click(callback)
  if (status) result.addClass(status)
  if (tail) result.append(tail)
  return result }

var rpc = function (tok,ty,req,cont) {
  obj = {}
  obj[ty] = [tok,req]
  var opts = {
    type:"PUT", url:"/", dataType:"json", processData:false, data:toJSON(obj)}
  $.ajax(opts).success(function(s) { cont(s) }) }

var addHabit = function(habit, cont) {
  return (function(){
    if (habit.match(/^[a-z]*$/)) {
      rpc(tok,"Update",{AddHabit:[user,habit]},cont) }}) }

var addHabitHandler = function() {
  var hab = document.getElementById("habit").value
  addHabit(hab, update)() }

var addNote = function(note, cont) {
  return (function(){
    if (!note || note == "") cont()
    else rpc(tok,"Update",{AddNote:[user,day,note]},cont) }) }

var addNoteHandler = function() {
  var not = document.getElementById("note").value
  addNote(not, update)() }

var trace = function(x) { console.log(x); return x }
var toJSON = function(s) { return JSON.stringify(s) }

var json = function(s) {
  if (s) {return JSON.parse(s) }
  else {return {}} }

var getHistory30 = function(cont) {
  rpc(tok,"Query",{History30:[user,day]},cont) }

var getChains = function(cont) {
  rpc(tok,"Query",{GetChains:[user,day]},cont) }

var delNote = function(note,cont) {
  return function() {
    rpc(tok,"Update",{DelNote:[user,day,note]},cont) }}

var delHabit = function(habit,cont) {
  return function() {
    rpc(tok,"Update",{DelHabit:[user,habit]},cont) }}

var getNotes = function(cont) {
  rpc(tok,"Query",{GetNotes:[user,day]},cont) }

var allHabits = function(cont) {
  rpc(tok,"Query",{ListHabits:user},cont) }

var habitsStatus = function(habitSet, day, cont) {
  rpc(tok,"Query",{"GetHabitsStatus":[user,day]},function(r){
    cont(r["STATUSES"]) }) }

var calcFailures = function(habitSet, successSet) {
  var result = []
  _.forEach(habitSet, function(habit){
    if (-1 === successSet.indexOf(habit))
      result.push(habit) })
  return result }

var update = function() {
  getHistory30(function(historyResponse) {
    var history = historyResponse["HISTORY"];
    getChains(function(chainsResponse) {
      chains = chainsResponse["CHAINS"]
      getNotes(function(noteResponse) {
        notes = noteResponse["NOTES"]
        allHabits(function(response) {
          var habitSet = response["HABITS"]
          habitsStatus(habitSet, day, function(statuses) {
            writeDOM(habitSet, statuses, notes, chains, history) }) }) }) }) }) }

var setDone = function(day, habit, statusCode, num, cont) {
  var status
  if (statusCode == "success") { status={"Success":num} }
  else if (statusCode == "failure") { status={"Failure":num} }
  else if (statusCode == "unspecified") { status={"Unspecified":[]} }
  else { throw "bad statusCode" }
  return (function() {
    rpc(tok,"Update",{"SetHabitsStatus":[user,day,habit,status]}, cont) }) }


var strSortInPlace = function (x) {
  x.sort(function(a,b){
    var t1 = "".concat(a)
    t1.toUpperCase()
    var t2 = "".concat(b)
    t2.toUpperCase()
    return t1.localeCompare(t2) }) }


var orWhat = function(n) {
  if (n===null || n.length===0) { return null }
  return n }

var habitClass = function(habit){
  if ("Success" in habit) return "success";
  if ("Failure" in habit) return "failure";
  if ("Unspecified" in habit) return "unspecified";
  return null; }

var writeDOM = function(habitSet, statuses, notes, chains, history) {
  var hdr = $("<h1>")
  hdr.addClass("header")
  var back = $("<span>&lt&lt</span>")
  var forward = $("<span>&gt&gt</span>")
  back.addClass("datechange")
  forward.addClass("datechange")
  back.click(shiftDate(-1, update))
  forward.click(shiftDate(1, update))
  hdr.append(back)
  var d = fromJulian(day)
  hdr.append(" "+d.getFullYear()+"/"+d.getMonth()+"/"+d.getDate()+" ")
  hdr.append(forward)

  var habitList = $("<p>")
  var habitNames = _.keys(statuses)
  strSortInPlace(habitNames)
  _.forEach(habitNames, function(nm){
    var status, alt, num=null
    if ("Success" in statuses[nm])
      { status="success"; alt="failure"; num=orWhat(statuses[nm]["Success"]) }
    else if ("Failure" in statuses[nm])
      { status="failure"; alt="unspecified"; num=orWhat(statuses[nm]["Failure"]) }
    else if ("Unspecified" in statuses[nm])
      { status="unspecified"; alt="success" }
    else
      { status="unspecified"; alt=true }
    var tail = null
    if (nm in chains) { tail = $("<sup>"); tail.text(chains[nm]) }
    var nameMe = $('<span style="white-space:nowrap">')
    var habitThing = habitDOM(nm, status, tail, setDone(day,nm,alt,0,update))
    if (null !== num) { habitThing.addClass("pairLeft") }
    nameMe.append(habitThing)
    if (null !== num) {
      var hack = $('<sub contenteditable="true">')
      onEnter(hack,function(){ hack.blur() })
      hack.blur(function(){
        setDone(day,nm,status,parseFloat(this.textContent),update)()})
      hack.text(num)
      hack.addClass("refutable")
      if (status) { hack.addClass(status) }
      hack.addClass("pairRight")
      nameMe.append(hack) }

    habitList.append(nameMe)
    habitList.append($("<span>"))
    })

  habitList.append(
    $('<input type="text">')
      .css("width","70")
      .addClass("refutable")
      .css("font-weight","bold")
      .val("addhabit")
      .change(function(){addHabit(this.value, update)()}))

  strSortInPlace(notes)
  var noteListP = $("<p>")
  var noteList = $("<ul>")
  noteList.addClass("note")
  noteListP.append(noteList)

  noteList.append(
    $('<li>').append(
      $('<input value="Add a Note" type="text">')
        .addClass("note")
        .change(function(x){ addNote(this.value, update)() }))
      .css("border-width","2")
      .addClass("note"))

  _.forEach(notes, function(note){
    noteList.append(noteDOM(note))})

  var historyDays = _.keys(history)
  historyDays.sort()
  while (historyDays.length > 30)
    historyDays.shift()
  console.log(historyDays)

  var percent = function(x) { return (x*100) + "%" }
  height = (10*habitNames.length)
  var historySvgDiv = $('<div>')
    .css({"width":"100%"})
    .css({"height":0})
    .css({"padding-bottom":percent(height/300)})
    // This padding-bottom thing is a hack to set the height relative to the
    // width.

  var historySvg = $('<svg>')
    .attr("width","100%")
    .attr("height","100%")
    .attr("viewbox","0 0 300 " + height)

  var i=0,j=0;
  _.forEach(habitNames, function(habit){
    j=0;
    _.forEach(historyDays,function(day){
      var x=j*10, y=i*10;
      console.log(i,j,x,100-y);
      historySvg.append($("<rect>")
        .attr("x",x).attr("y",y)
        .attr("width",10).attr("height",10)
        .addClass(habitClass(history[day][habit]))
        .addClass("refutable"))
      j++; })
    i++ })

  historySvgDiv.append(historySvg)
  historySvgDiv.html(historySvgDiv.html())

  var node = $("#notices")
  node.empty()
  node.append(hdr)
  node.append(habitList)
  node.append(noteListP)
  node.append(historySvgDiv) }

var data =
	[ {author:"Ben", text:"Sup, bitches?"}
	, {author:"Colton", text:"..."}
	]

var Comment = React.createClass({render: function() {
	return (<div className="comment">
		<h2 className="commentAuthor">
			{this.props.author}
			</h2>
		<pre>{this.props.children}</pre>
	</div> )}});

var CommentList = React.createClass({render: function() {
	var commentNodes = this.props.data.map(function (comment) {
		return <Comment author={comment.author}>{comment.text}</Comment>; })
	return (<div className="commentList">
		{commentNodes}
		</div>)}});

var CommentForm = React.createClass({render: function() {
	return (<div className="commentForm">
		<h1>Hello, world! I am a CommentForm.</h1>
		</div> )}});

var CommentBox = React.createClass({render: function() {
  return (<div className="commentBox">
    <h1>Comments</h1>
    <CommentList data={this.props.data}/>
    <CommentForm />
    </div> )}});

React.renderComponent(
	<CommentBox data={data} />,
	document.getElementById('app'));


// var alreadyAuthenticated = (user && tok)

// if (alreadyAuthenticated) { update() } else {
  // user = prompt("Username","user");
  // (function () {
    // var pass = prompt("Password","")
    // var obj = {"Register":[user,pass]}
    // var opts = {
      // type:"POST", url:"/", dataType:"json", processData:false, data:toJSON(obj)}
    // $.ajax(opts).success(function(response) {
      // tok=response["AUTH"]
      // localStorage.setItem("token",tok)
      // localStorage.setItem("username",user)
      // update() })})()}
