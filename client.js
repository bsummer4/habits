/**
 * @jsx React.DOM
 */


//////////////////////////////////////////////////////////////////////////////
// Pure Functions
Date.prototype.modifiedJulianDay = function() {
  return Math.floor((this/86400000)-(this.getTimezoneOffset()/1440)+40587) }

var normalizeNote = function(note) { return note.replace(/\s+/g,' ').trim() }
var julian = function(d) { return d.modifiedJulianDay() }
var epoch = julian(new Date(0))
var fromJulian = function(j) { return new Date(86400000*(j-epoch)) }
var day = julian(new Date())
var member = function(i,a) { return !(-1 === a.indexOf(i)) }
var cssPercent = function(x) { return (x*100) + "%" }
var concat = function(arrays) { return [].concat.apply([],arrays); }


//////////////////////////////////////////////////////////////////////////////
// React Components
var LoginForm = React.createClass({
  handleSubmit: function() {
    user = this.refs.user.getDOMNode().value.trim()
    var pass = this.refs.pass.getDOMNode().value.trim()
    if (user && pass) this.props.login(user,pass)
    return false; },
  render: function() {
    return (<form className="loginForm" onSubmit={this.handleSubmit}>
      <input type="text" ref="user" placeholder="Username"></input>
      <input type="Password" ref="pass" placeholder="Password"></input>
      <input type="submit" value="Login/Register" />
      </form> )} })

var LogoutForm = React.createClass({
  handleSubmit: function () {
    this.props.logout(); return false; },
  render: function() {
    return (<form className="logoutForm" onSubmit={this.handleSubmit}>
      <input type="submit" value="Logout" />
      </form> )}})

var AddNoteForm = React.createClass({
  handleSubmit: function () {
    var note = this.refs.name.getDOMNode().value.trim();
    if (note === "") return false;
    this.props.addNote(note);
    return false; },
  render: function() {
    return (<form onSubmit={this.handleSubmit}>
      <input placeholder="Add a Note" ref="name" className="note" type="text" />
      </form>) }})

var Note = React.createClass({
  submit: function() {
    var fresh = normalizeNote(this.refs.name.getDOMNode().value)
    var old = normalizeNote(this.props.name)
    if (fresh == old) { return false }
    this.props.renameNote(old,fresh);
    return false; },
  render: function() { return (
    <form onSubmit={this.submit}>
      <input ref="name" className="note" defaultValue={this.props.name} />
      </form> )}})

var Notes = React.createClass({
  noteLI: function(note) { return(
    <li className="note">
      <Note name={note} renameNote={this.props.renameNote} />
      </li> )},
  render: function() { return(
    <p><ul className="note">
      <li className="note" style={{"border-width":2}}>
        <AddNoteForm addNote={this.props.addNote} /></li>
      {this.props.noteList.map(this.noteLI)}
      </ul></p> )}})

// <HistoryRect x y status day habit>
var HistoryRect = React.createClass({
  render: function() {
    var x = 10*this.props.x;
    var y = 10*this.props.y;
    var cx = ['refutable', this.props.status].join(" ")
    return <rect x={x} y={y} width={10} height={10} className={cx} />}})

// The ‘padding-bottom’ css is a hack to set the height relative to the width.
var History = React.createClass({
  render: function() {
    var d = this.props.habitData
    if (0 === d.length) { return <div /> }
    width = 10 * d[0].length;
    height = 10 * d.length;
    var viewbox = [0,0,width,height].join(" ")
    var relHeight = cssPercent(height/width)
    return <div style={{width:"100%", height:0, "padding-bottom":relHeight}}>
      <svg width={"100%"} height={"100%"} viewBox={viewbox}>
        {_.map(d, function(row,j){
          return _.map(row, function(cell,i){
            var day=cell[0], habit=cell[1], status=cell[2];
            return <HistoryRect
              x={i} y={j} status={status} day={day} habit={habit} /> })})}
        </svg>
      </div> }})


//////////////////////////////////////////////////////////////////////////////
// Global State and Operations on It
var tok = localStorage.getItem("token")
var user = localStorage.getItem("username")
var shiftDate = function(n,cont){ return(function(){ day+=n; cont() })}
onEnter = function(j,f) {
  j.on("keyup",function(e){
    if (e.keyCode == 13)
      f() })
  return j }

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

var addNote = function(note, cont) {
  note = normalizeNote(note);
  return (function(){
    if (!note || note == "") cont()
    else rpc(tok,"Update",{AddNote:[user,day,note]},cont) }) }

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

var fsort = function(s) { strSortInPlace(s); return s; }
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
      .attr("placeholder", "addhabit")
      .change(function(){addHabit(this.value, update)()}))

  var node = $("#notices")
  node.empty()
  node.append(hdr)
  node.append(habitList)
  node.append($('<div id="noteListPlaceholder">'))
  node.append($('<div id="historyPlaceholder">'))

  var addNoteFn = function (name) { addNote(name, update)(); }
  var renameFn = function (a,b) { delNote(a,addNote(b,update))() }
  React.renderComponent(
    <Notes addNote={addNoteFn} renameNote={renameFn} noteList={fsort(notes)} />,
    document.getElementById("noteListPlaceholder"));

  var d = _.map(fsort(_.keys(statuses)), function(habit,i){
    return _.map(fsort(_.keys(history)), function(day,j){
      return [day, habit, habitClass(history[day][habit])] })})

  React.renderComponent(
    <History habitData={d} />,
    document.getElementById("historyPlaceholder")); }


// Inject CSS
var stylesheet = "form {margin:0}"
var node = document.createElement('style')
node.innerHTML = stylesheet
document.body.appendChild(node)


// Render the page.
main = function() {
  tok = localStorage.getItem("token")
  user = localStorage.getItem("username")
  React.unmountComponentAtNode(document.getElementById('auth'));
  $("#notices").empty()

  var login = function (user,pass) {
    var opts =
      { type:"POST", url:"/", dataType:"json", processData:false
      , data:toJSON({"Register":[user,pass]}) }
    $.ajax(opts).success(function(response) {
      console.log(response);
      if ("AUTH" in response) {
        tok=response["AUTH"]
        localStorage.setItem("token",tok)
        localStorage.setItem("username",user)
        console.log(user,pass,tok);
        main() }})}

  var logout = function() {
    var tok = localStorage.removeItem("token")
    var user = localStorage.removeItem("username")
    main() }

  var alreadyAuthenticated = (user && tok)

  if (alreadyAuthenticated) {
    console.log("hi!")
    React.renderComponent(
      <LogoutForm logout={logout} />,
      document.getElementById('auth'))
    update() }

  else {
    React.renderComponent(
      <div>
        Please login or register. If you try to
        login with a username that doesn't exist,
        the we will register you instead.
        <LoginForm login={login}/>
        </div>,
      document.getElementById('auth')) }};

main();
