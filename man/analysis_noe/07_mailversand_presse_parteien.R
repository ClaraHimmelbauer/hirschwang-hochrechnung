# Mailversand

adressen <- "barbara.riener@stmk.gv.at; 	detlev.eisel-eiselsberg@stvp.at; 	ltk-spoe@stmk.gv.at; 	guenter.pirker@spoe.at; 	mkunasek@fpoe-stmk.at; 	anton.kogler@fpoe-stmk.at; 	sandra.krautwaschl@gruene.at; 	timon.scheuer@gruene.at​; 	claudia.klimt-weithaler@landtag.steiermark.at; 	a.fuchs@kpoe-steiermark.at; 	niko.swatek@neos.eu; 	anton.tropper@neos.eu; 	klaus.schneeberger@noel.gv.at; 	bernhard.ebner@vpnoe.at; 	reinhard.hundsmueller@spoe.at; 	wolfgang.kocevar@spoe.at; 	udo.landbauer@fpoe.at; 	andreas.spanring@fpoe.at; 	helga.krismer@gruene.at; 	hikmet.arslan@gruene.at; 	indra.collini@neos.eu; 	benjamin.hubijar@neos.eu; 	christian.doerfel@ooe.gv.at; 	florian.hiegelsberger@ooevp.at; 	michael.lindner@ooe.gv.at; 	florian.koppler@spoe.at; 	herwig.mahr@ooe.gv.at; 	hubert.schreiner@fpoe.at; 	severin.mayr@gruene.at; 	ursula.roschger@gruene.at; 	felix.eypeltauer@neos.eu; 	johannes.egger@neos.eu; 	buergermeister@umhausen.gv.at; 	s.kolland@tiroler-vp.at; 	spoe.landtagsklub@tirol.gv.at; 	lukas.matt@spoe.at; 	markus.abwerzger@fpoe.tirol; 	julia.pelech@fpoe.at; 	gebi.mair@gruene.at; 	natascha.chmelar@gruene.at; 	dominik.oberhofer@neos.eu; 	ines.mairhofer@neos.eu; 	markus.sint@liste-fritz.at; 	andrea.haselwanter-schneider@liste-fritz.at; 	wolfgang.mayer@salzburg.gv.at; 	stampfer@oevp-sbg.at; 	michael.wanner@salzburg.gv.at; 	gerald.forcher@spoe.at; 	egger.david@spoe.at; 	marlene.svazek@fpoe.at; 	hermann.kirchmeier@fpoe.at; 	kimbie.humer-vogl@salzburg.gv.at; 	simon.hofbauer@gruene.at; 	liesl.weitgasser@neos.eu; 	nikolaus.glaser@neos.eu; 	markus.malle@oevpclub.at; 	julia.loeschnig@oevpkaernten.at; 	herwig.seiser@spoe.at; 	andreas.sucher@spoe.at; 	gernot.darmann@ktn-landtag.at; 	t.schweiger@freiheitliche-ktn.at; 	olga.voglauer@gruene.at; 	stefan.samonig@gruene.at; 	janos.juvan@neos.eu; 	gerhard.koefer@team-kaernten.at; 	markus.woelbitsch@wien.oevp.at; 	markus.keschmann@wien.oevp.at; 	josef.taucher@spw.at ; 	barbara.novak@spw.at; 	maximilian.krauss@fpoe.at; 	andreas.guggenberger@fpoe.at; 	david.ellensohn@gruene.at; 	christian.tesar@gruene.at; 	bettina.emmerling@neos.eu; 	philipp.kern@neos.eu; 	roland.fruehstueck@volkspartei.at; 	dietmar.wetz@volkspartei.at; 	lukas.riepler@spoe.at; 	klaus.gasser@spoe.at; 	christof.bitschi@vfreiheitliche.at; 	dominik.hagen@vfreiheitliche.at; 	eva.hammerer@gruene.at; 	jessica.boesch@gruene.at; 	sabine.scheffknecht@neos.eu; 	simon.muchitsch@neos.eu; 	martin.kotynek@derstandard.at; 	florian.asamer@diepresse.com; 	c.oistric@heute.at; 	c.nusser@heute.at; 	hubert.patterer@kleinezeitung.at; 	wolfgang.fercher@kleinezeitung.at; 	chefredaktion@kronenzeitung.at; 	martina.salomon@kurier.at; 	w.fahrnberger@noen.at; 	d.lohninger@noen.at; 	chefredaktion@nachrichten.at; 	n.fellner@oe24.at; 	manfred.perterer@sn.at; 	mario.zenhaeusern@tt.com; 	alois.vahrner@tt.com; 	gerold.riedmann@vn.at; 	judith.belfkih@wienerzeitung.at; 	m.stefanitsch@bvz.at; 	r.korntner@volksblatt.at; 	moritz.moser@neue.at; 	chefredaktion@apa.at; 	oberoesterreich@apa.at; 	tirol@apa.at; 	niederoesterreich@apa.at; 	wien@apa.at; 	burgenland@apa.at; 	salzburg@apa.at; 	kaernten@apa.at; 	vorarlberg@apa.at; 	steiermark@apa.at; 	michael.voelker@derstandard.at; 	oliver.pink@diepresse.com; 	michael.jungwirth@kleinezeitung.at; 	politik@kronenzeitung.at; 	daniela.kittner@kurier.at; 	l.roehrer@noen.at; 	politik@nachrichten.at; 	andreas.koller@sn.at; 	Birgit.Entner-Gerold@vn.at; 	simon.rosner@wienerzeitung.at; 	w.millendorfer@bvz.at; 	m.ebert@volksblatt.at; 	innenpolitik@apa.at; "

if(gueltig == TRUE){
  text <- paste(sep = "\n",
                paste0("Sehr geehrte Damen und Herren,"),
                "",
                "hier finden Sie aktuelle OGM-Hochrechnung für Servus-TV.",
                "",
                paste0("Uhrzeit: ", format(Sys.time(), "%H:%M")),
                paste0("Maximale Schwankungsbreite: ", sb_str),
                "",
                paste0("ÖVP: ",      proz_str[1]),
                paste0("SPÖ: ",      proz_str[2]),
                paste0("FPÖ: ",      proz_str[3]),
                paste0("Grüne: ",    proz_str[4]),
                paste0("NEOS: ",     proz_str[5]),
                paste0("Sonstige: ", proz_str[6]),
                "",
                "Mit freundlichen Grüßen",
                "Johannes Klotz",
                "",
                paste0("Diese Mail wurde automatisch generiert."))
  
  OutApp <- COMCreate("Outlook.Application")
  outMail = OutApp$CreateItem(0)
  outMail[["To"]] = "office@ogm.at"
  outMail[["subject"]] = paste0(format(Sys.time(), "%H:%M"), " - OGM-Hochrechnung NÖ-Landtagswahl für Servus-TV")
  outMail[["body"]] = text
  outMail[["bcc"]] = "dorner@ogm.at; himmelbauer@ogm.at; klotz@ogm.at"
  # vor Sendungsbeginn einkommentieren!
  # outMail[["bcc"]] = adressen
 # outMail$Send()
}
