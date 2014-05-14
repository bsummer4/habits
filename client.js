/**
 * @jsx React.DOM
 */

// TODO Compute chains instead of requesting them.
// TODO The SVG element is not updated.
// TODO Do smaller queries to the server.
//   If we change adherance data for today, refresh today's habit info.
//   If we change notes data for today, refresh today's note info.
//   If we change days, update the display right away, but request more day
//     information if we don't have enough for today and the last 30 days.
//   When we first load, do a series of updates.

// data Status = "success" | "failure" | "unspecified"
// type Habit = String
// type Token = String
// type User = String
// type Day = Int // Modified Julian Day
// type Adherence = Day → Habit →
//   (status:Status, num:Maybe Number, chains:Number)
// type State = {day:Day user:User, tok:Token, days:Adherence}


//////////////////////////////////////////////////////////////////////////////
// Pure Functions
Date.prototype.modifiedJulianDay = function(){
  return Math.floor((this/86400000)-(this.getTimezoneOffset()/1440)+40587) }

var arb = function(d) { return d[_.keys(d)[0]]; }
var normalizeNote = function(note) { return note.replace(/\s+/g,' ').trim() }
var julian = function(d) { return d.modifiedJulianDay() }
var epoch = julian(new Date(0))
var fromJulian = function(j) { return new Date(86400000*(j-epoch)) }
var day = julian(new Date())
var member = function(i,a) { return !(-1 === a.indexOf(i)) }
var cssPercent = function(x) { return (x*100) + "%" }
var concat = function(arrays) { return [].concat.apply([],arrays) }
var orWhat = function(n) { (n===null || n.length===0) ? null : n }
var statusCycle = function (st) {
  if (st == "success") return "failure"
  if (st == "failure") return "unspecified"
  if (st == "unspecified") return "success"
  return null }

var habitNum = function(habit){
  var x = null
  if ("Success" in habit) { x=habit["Success"] }
  if ("Failure" in habit) { x=habit["Failure"] }
  if ("Unspecified" in habit) { x=habit["Unspecified"] }
  if ("number" != typeof x) { x=null }
  return x }

var habitClass = function(habit){
  if ("Success" in habit) { return "success" }
  if ("Failure" in habit) { return "failure" }
  if ("Unspecified" in habit) { return "unspecified" }
  return null }

var getDay = function(days,day) { return (day in days) ? days[day] : {}}
var getHabit = function(habits,nm) {
  var blank={status:"unspecified", num:null, chains:0}
  return ((nm in habits) ? habits[nm] : blank) }

var responsesToState = function(habitSet, notes, chains, history) {
  var today = day
  var days = {}
  _.forEach(history, function(_,day){ days[day]={} })
  _.forEach(habitSet, function(habit) {
    _.forEach(history, function(_, day) {
      days[day][habit] =
        { status: habitClass(history[day][habit])
        , num: habitNum(history[day][habit])
        , chains: 0 }})})
  _.forEach(habitSet, function(habit) {
    if (habit in chains) {
      days[today][habit].chains = chains[habit] }})
  return {day:day, user:USER, tok:TOK, days:days} }


//////////////////////////////////////////////////////////////////////////////
// Utility Procedures
var strSortInPlace = function (x) {
  x.sort(function(a,b){
    var t1 = "".concat(a)
    t1.toUpperCase()
    var t2 = "".concat(b)
    t2.toUpperCase()
    return t1.localeCompare(t2) }) }

var fsort = function(s) { strSortInPlace(s); return s }


//////////////////////////////////////////////////////////////////////////////
// React Components
var LoginForm = React.createClass({
  handleSubmit: function(){
    USER = this.refs.user.getDOMNode().value.trim()
    var pass = this.refs.pass.getDOMNode().value.trim()
    if (USER && pass) this.props.login(USER,pass)
    return false; },
  render: function(){
    return (<form className="loginForm" onSubmit={this.handleSubmit}>
      <input type="text" ref="user" placeholder="Username"></input>
      <input type="Password" ref="pass" placeholder="Password"></input>
      <input type="submit" value="Login/Register" />
      </form> )} })

var LogoutForm = React.createClass({
  handleSubmit: function (){
    this.props.logout(); return false; },
  render: function(){
    return (<form className="logoutForm" onSubmit={this.handleSubmit}>
      <input type="submit" value="Logout" />
      </form> )}})

var AddNoteForm = React.createClass({
  handleSubmit: function (){
    var note = this.refs.name.getDOMNode().value.trim();
    if (note === "") return false;
    this.props.addNote(note);
    return false; },
  render: function(){
    return (<form onSubmit={this.handleSubmit}>
      <input placeholder="Add a Note" ref="name" className="note" type="text" />
      </form>) }})

var Note = React.createClass({
  submit: function(){
    var fresh = normalizeNote(this.refs.name.getDOMNode().value)
    var old = normalizeNote(this.props.name)
    if (fresh == old) { return false }
    this.props.renameNote(old,fresh);
    return false; },
  render: function(){ return (
    <form onSubmit={this.submit}>
      <input ref="name" className="note" defaultValue={this.props.name} />
      </form> )}})

var Notes = React.createClass({
  noteLI: function(note) { return(
    <li className="note">
      <Note name={note} renameNote={this.props.renameNote} />
      </li> )},
  render: function(){ return(
    <p><ul className="note">
      <li className="note" style={{"border-width":2}}>
        <AddNoteForm addNote={this.props.addNote} /></li>
      {this.props.noteList.map(this.noteLI)}
      </ul></p> )}})

// <HistoryRect x y status day habit>
var HistoryRect = React.createClass({
  render: function(){
    var x = 10*this.props.x;
    var y = 10*this.props.y;
    var cx = ['refutable', this.props.status].join(" ")
    return <rect x={x} y={y} width={10} height={10} className={cx} /> }})

// The ‘padding-bottom’ css is a hack to set the height relative to the width.
var History = React.createClass({
  componentDidUpdate: function(a,b){
    document.getElementById("svghack").innerHTML=this.ihtml },

  render: function(){
    var d = this.props.habitData
    var days = fsort(_.keys(d))
    if (0 === days.length) { return <div /> }
    var habits = fsort(_.keys(d[days[0]]))
    if (0 === habits.length) { return <div /> }
    var width = 10 * days.length;
    var height = 10 * habits.length;
    var viewbox = [0,0,width,height].join(" ")
    var relHeight = cssPercent(height/width)
    var today = this.props.today;
    var info = concat(
        _.map(habits, function(hab,j){
          return (_.map(_.range(today-29,today+1), function(day,i){
            var s = getHabit(getDay(d,day),hab).status;
            return {x:i,y:j,s:s,day:day,hab:hab} }))}))

    this.ihtml = React.renderComponentToString(
      <svg width="100%" height="100%" viewBox={viewbox}>
        {info.map(function(r) { return(
          <HistoryRect x={r.x} y={r.y} status={r.s} day={r.day} habit={r.hab}/>
          )})}
        </svg>)

    return <div style={{width:"100%", height:0, "padding-bottom":relHeight}}>
      <div id="svghack" />
      </div> }})

var DayNav = React.createClass({
  dayStr: function (d) { return (
    " " + d.getFullYear() + "/" + d.getMonth() + "/" + d.getDate() + " ")},
  render: function(){
    return (<h1 className="header">
      <span className="datechange" onClick={this.props.back}>&lt;&lt;
        </span>
      {this.dayStr(fromJulian(this.props.day))}
      <span className="datechange" onClick={this.props.next}>&gt;&gt;
        </span>
      </h1> )}})

// <Habit name chain num callback>
var Habit = React.createClass({render: function(){
  cx = ["refutable", this.props.status].join(" ")
  return(<span>
    <span style={{"white-space":"nowrap"}}>
      <span className={"pairLeft "+cx} onClick={this.props.callback}>
        {this.props.name}
        {this.props.chain ? [<sup>{this.props.chain}</sup>] : []}
        </span>
      {null!==this.props.num ? [<sub className={"pairRight "+cx}>
                                  {this.props.num}</sub>] : []}
      </span>
    &#8203;</span> )}})

var AddHabit = React.createClass({render: function(){
  return (<input
    className="refutable" placeholder="addhabit"
    style={{width:"70px", "font-weight":"bold"}} /> )}})

// habitInfo ∷ habit → {status chain num}
// <HabitList habitInfo>
var HabitList = React.createClass({render: function (){
  var d = this.props.habitInfo // Day → {status num chains}
  var names = fsort(_.keys(d))
  var cb = function(){ console.log("Placeholder callback!"); }
  return (<p>
    {_.map(names, function (nm) { return(
      <Habit
        status={getHabit(d,nm).status}
        name={nm}
        chain={getHabit(d,nm).chains}
        num={getHabit(d,nm).num}
        callback={cb} /> )})}
    <span> </span>
    <AddHabit />
    </p> )}})

var App = React.createClass({
  shiftDate: function(n) {
    var x=this;
    return function(){
      x.setState({day:x.state.day+n}) }},

  getInitialState: function() {
    var user = localStorage.getItem("username")
    var tok = localStorage.getItem("token")
    var day = julian(new Date())
    var nulls = {day:day, tok:tok, user:user, days:{}}
    var fuckjs = this;
    if (user && tok) {
      getUpdates(user, tok, day, function(x){fuckjs.setState(x)}) }
    return nulls; },

  render: function(){
    var st = this.state
    if (!(st.user && st.tok)) { return(
      <div>
        Please login or register. If you try to login with a username that
        doesn't exist, the we will register you instead.
        <LoginForm login={login}/>
        </div> )}
    return(
      <ul>
        <DayNav day={st.day} next={this.shiftDate(1)} back={this.shiftDate(-1)}
          />
        <HabitList habitInfo={getDay(st.days,st.day)} />
        <History habitData={st.days} today={st.day} />
        <LogoutForm logout={logout} />
        </ul> )}})


///////////////////////////////////////////////////////////////////////////////
// RPC Calls
var rpc = function (tok,ty,req,cont) {
  obj = {}
  obj[ty] = [tok,req]
  var opts = {
    type:"PUT", url:"/", dataType:"json", processData:false, data:toJSON(obj)}
  $.ajax(opts).success(function(s) { cont(s) }) }

var addHabit = function(habit, cont) {
  return (function(){
    if (habit.match(/^[a-z]*$/)) {
      rpc(TOK,"Update",{AddHabit:[USER,habit]},cont) }}) }

var addNote = function(note, cont) {
  note = normalizeNote(note);
  return (function(){
    if (!note || note == "") cont()
    else rpc(TOK,"Update",{AddNote:[USER,day,note]},cont) }) }

var toJSON = function(s) { return JSON.stringify(s) }

var json = function(s) {
  if (s) {return JSON.parse(s) }
  else {return {}} }

var getHistory30 = function(cont) {
  rpc(TOK,"Query",{History30:[USER,day]},cont) }

var getChains = function(cont) {
  rpc(TOK,"Query",{GetChains:[USER,day]},cont) }

var delNote = function(note,cont) {
  return function(){
    rpc(TOK,"Update",{DelNote:[USER,day,note]},cont) }}

var delHabit = function(habit,cont) {
  return function(){
    rpc(TOK,"Update",{DelHabit:[USER,habit]},cont) }}

var getNotes = function(cont) {
  rpc(TOK,"Query",{GetNotes:[USER,day]},cont) }

var allHabits = function(cont) {
  rpc(TOK,"Query",{ListHabits:USER},cont) }

var habitsStatus = function(habitSet, day, cont) {
  rpc(TOK,"Query",{"GetHabitsStatus":[USER,day]},function(r){
    cont(r["STATUSES"]) }) }

var setDone = function(day, habit, statusCode, num, cont) {
  var status
  if (statusCode == "success") { status={"Success":num} }
  else if (statusCode == "failure") { status={"Failure":num} }
  else if (statusCode == "unspecified") { status={"Unspecified":[]} }
  else { throw "bad statusCode" }
  return (function(){
    rpc(TOK,"Update",{"SetHabitsStatus":[USER,day,habit,status]}, cont) }) }


//////////////////////////////////////////////////////////////////////////////
// Global State and Operations on It
var TOK = localStorage.getItem("token")
var USER = localStorage.getItem("username")

var getUpdates = function(user, tok, day, cont){
  getHistory30(function(historyResponse) {
    var history = historyResponse["HISTORY"]
    getChains(function(chainsResponse) {
      chains = chainsResponse["CHAINS"]
      getNotes(function(noteResponse) {
        notes = noteResponse["NOTES"]
        allHabits(function(response) {
          var habitSet = response["HABITS"]
          var f = responsesToState;
          cont(f(habitSet,notes,chains,history)) })})})})}


//////////////////////////////////////////////////////////////////////////////
// Inject CSS
var stylesheet =
	[ "form {margin:0}"
	, "div.toplevel { max-width:480; width:95%; margin:0 auto; }"
	, "ul#notices { padding: 0; }"
	, "h1.header { text-align:center; }"
	, ".refutable {"
	, "  border-width:1; border-style:solid;"
	, "  padding:2px; margin:1px; display:inline-block; }"
	, "	rect.refutable {"
	, "		stroke-width: 0.3;"
	, "		stroke: black; }"
	, ".pairLeft { margin-right:0 }"
	, ".pairRight { margin-left:-2; }"
	, ".history { cellpadding:0; cellspacing:0; spacing:0; padding:0; border:0; }"
	, "table.history { margin:0 auto; }"
	, "div.historyTD { min-width:13; min-height:13 }"
	, "td.history { border: 0px solid }"
	, ".datechange { forground-color:blue; text-decoration:underline; }"
	, ".success { background-color:#88ff88; border-color:#228822; }"
	, ".failure { background-color:#ff8888; border-color:#882222; }"
	, ".unspecified { border: 0px solid #9999dd; background-color:#ddddff }"
	, ".refutable:hover { background-color:yellow; }"
	, "td.history { border: 0px solid white }"
	, "rect.success { fill: green; }"
	, "rect.failure { fill: red; }"
	, "rect.unspecified { fill: #ddddff; }"
	, "rect.refutable:hover { fill: yellow; }"
	, "input.note { width:100%; border-width:0; }"
	, "ul.note { padding: 0; list-style: none; }"
	, "input.note { background-color:inherit; }"
	, "li.note:hover { background-color:yellow; }"
	, "input.note:focus { outline: 0; }"
	, "li.note {"
	, "  padding: 3px;"
	, "  margin: 2px;"
	, "  display: inline-block;"
	, "  width: 97%;"
	, "  border-width: 0 2px 0 2px;"
	, "  border-style: solid; }"
	, "div.tooltip::before {"
	, "  content: attr(data-tip);"
	, "  font-size: 10px;"
	, "  position:absolute;"
	, "  z-index: 999;"
	, "  white-space:nowrap;"
	, "  bottom:9999px;"
	, "  left: 50%;"
	, "  background:#000;"
	, "  color:#e0e0e0;"
	, "  padding:0px 7px;"
	, "  line-height: 24px;"
	, "  height: 24px;"
	, "  opacity: 0;"
	, "  transition:opacity 0.4s ease-out; }"
	, "div.tooltip:hover::before { opacity: 1; bottom:-35px; }"
	]

var node = document.createElement('style')
node.innerHTML = stylesheet.join("\n")
document.body.appendChild(node)


//////////////////////////////////////////////////////////////////////////////
// Render the page.
logout = function(){
  localStorage.removeItem("token")
  localStorage.removeItem("username")
  main() }

login = function (user,pass) {
  var opts =
    { type:"POST", url:"/", dataType:"json", processData:false
    , data:toJSON({"Register":[user,pass]}) }
  $.ajax(opts).success(function(response) {
    if ("AUTH" in response) {
      TOK=response["AUTH"]
      localStorage.setItem("token",TOK)
      localStorage.setItem("username",user)
      main() }})}

main = function(){
	USER = localStorage.getItem("username")
	TOK = localStorage.getItem("token")
	React.unmountComponentAtNode(document.getElementById('notices'))
  React.renderComponent(<App />, document.getElementById('notices')) }

main()
