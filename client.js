/**
 * @jsx React.DOM
 */

// TODO Compute chains instead of requesting them.
// TODO Allow incomplete ‘state.days’ records.
//   TODO For missing nodes assume blank={status:"unspecified", num:null, chains:0}
//   TODO For missing days, assume ‘map (\habit→(habit,blank)) habitSet’
//   TODO Don't pad out ‘state.days’ records with blank entries.
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
  // console.log(habit,x)
  return x }

var habitClass = function(habit){
  if ("Success" in habit) { return "success" }
  if ("Failure" in habit) { return "failure" }
  if ("Unspecified" in habit) { return "unspecified" }
  return null }

var responsesToState = function(habitSet, notes, chains, history) {
  console.log("toState", habitSet, notes, chains, history)
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
      console.log("sup", habit,chains)
      days[today][habit].chains = chains[habit] }})
  _.forEach(habitSet, function(habit) {
    _.forEach(history, function(_, day) {
      if (!(habit in days[day])) {
        days[day][habit] = {chains:0, status:"unspecified", num:null } }})})
  console.log("woot!")
  return {day:day, user:user, tok:tok, days:days} }


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
    user = this.refs.user.getDOMNode().value.trim()
    var pass = this.refs.pass.getDOMNode().value.trim()
    if (user && pass) this.props.login(user,pass)
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
    // console.log("rect", this.props)
    var x = 10*this.props.x;
    var y = 10*this.props.y;
    var cx = ['refutable', this.props.status].join(" ")
    return <rect x={x} y={y} width={10} height={10} className={cx} />}})

// The ‘padding-bottom’ css is a hack to set the height relative to the width.
var History = React.createClass({
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

    return <div style={{width:"100%", height:0, "padding-bottom":relHeight}}>
      <svg width={"100%"} height={"100%"} viewBox={viewbox}>
        {_.map(days, function(day,i){ return(
          _.map(habits, function(hab,j){
            var s = d[day][hab].status;
            return <HistoryRect
              x={i} y={j} status={s} day={day} habit={hab} /> }))})}
        </svg>
      </div> }})

var update = function() { return null; }

var DayNav = React.createClass({
  dayStr: function (d) { return (
    " " + d.getFullYear() + "/" + d.getMonth() + "/" + d.getDate() + " ")},
  back: function(){},
  forward: function(){},
  render: function(){
    return (<h1 className="header">
      <span className="datechange" onClick={shiftDate(-1,update)}>&lt;&lt;
        </span>
      {this.dayStr(fromJulian(this.props.day))}
      <span className="datechange" onClick={shiftDate(1,update)}>&gt;&gt;
        </span>
      </h1> )}})

// <Habit name chain num callback>
var Habit = React.createClass({render: function(){
  // console.log("hihi",this.props);
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
  var cb = function(){ console.log("clicked!"); }
  return (<p>
    {_.map(names, function (nm) { return(
      <Habit
        status={d[nm].status}
        name={nm}
        chain={d[nm].chains}
        num={d[nm].num}
        callback={cb} /> )})}
    <span> </span>
    <AddHabit />
    </p> )}})

var App = React.createClass({
  getInitialState: function() {
    var user = localStorage.getItem("username")
    var tok = localStorage.getItem("token")
    var day = julian(new Date())
    var nulls = {day:day, tok:tok, user:user, days:{}}
    console.log(user,tok,day)
    var fuck = this;
    if (user && tok) {
      getUpdates(user, tok, day, function(x){fuck.setState(x)}) }
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
        <DayNav day={st.day} />
        <HabitList habitInfo={st.days[st.day]} />
        <History habitData={st.days} />
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
  return function(){
    rpc(tok,"Update",{DelNote:[user,day,note]},cont) }}

var delHabit = function(habit,cont) {
  return function(){
    rpc(tok,"Update",{DelHabit:[user,habit]},cont) }}

var getNotes = function(cont) {
  rpc(tok,"Query",{GetNotes:[user,day]},cont) }

var allHabits = function(cont) {
  rpc(tok,"Query",{ListHabits:user},cont) }

var habitsStatus = function(habitSet, day, cont) {
  rpc(tok,"Query",{"GetHabitsStatus":[user,day]},function(r){
    cont(r["STATUSES"]) }) }

var setDone = function(day, habit, statusCode, num, cont) {
  var status
  if (statusCode == "success") { status={"Success":num} }
  else if (statusCode == "failure") { status={"Failure":num} }
  else if (statusCode == "unspecified") { status={"Unspecified":[]} }
  else { throw "bad statusCode" }
  return (function(){
    rpc(tok,"Update",{"SetHabitsStatus":[user,day,habit,status]}, cont) }) }


//////////////////////////////////////////////////////////////////////////////
// Global State and Operations on It
var tok = localStorage.getItem("token")
var user = localStorage.getItem("username")
var shiftDate = function(n,cont){ return(function(){ day+=n; cont() })}

var getUpdates = function(user, tok, day, cont){
  console.log("getUpdates!", user,tok,day);
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
var stylesheet = "form {margin:0}"
var node = document.createElement('style')
node.innerHTML = stylesheet
document.body.appendChild(node)


//////////////////////////////////////////////////////////////////////////////
// Render the page.
var logout = null;
var login = null;
var top = null;
main = function(){
  tok = localStorage.getItem("token")
  user = localStorage.getItem("username")
  React.unmountComponentAtNode(document.getElementById('auth'))
  React.unmountComponentAtNode(document.getElementById('notices'))
  login = function (user,pass) {
    var opts =
      { type:"POST", url:"/", dataType:"json", processData:false
      , data:toJSON({"Register":[user,pass]}) }
    $.ajax(opts).success(function(response) {
      // console.log(response)
      if ("AUTH" in response) {
        tok=response["AUTH"]
        localStorage.setItem("token",tok)
        localStorage.setItem("username",user)
        // console.log(user,pass,tok)
        main() }})}
  logout = function(){
    var tok = localStorage.removeItem("token")
    var user = localStorage.removeItem("username")
    main() }
  React.renderComponent(<App />, document.getElementById('notices')) }

main()
