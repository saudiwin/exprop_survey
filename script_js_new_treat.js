Qualtrics.SurveyEngine.addOnload(function()
{
  
  // Javascript file for randomizing conjoint experiment
  // Author Robert Kubinec, May-1-2017
  // For questions, contact rmk7xy@virginia.edu
  
  function commarize() {
    // Alter numbers larger than 1k
    
    // make positive if negative for conversion
    
    if (this < 0) {
      var new_this = this * -1;
      var sign_change = 1;
    } else {
      var sign_change = 0;
      var new_this = this;
    }
    
    if (new_this >= 1e3) {
      var units = ["k", "M", "B", "T"];
      
      // Divide to get SI Unit engineering style numbers (1e3,1e6,1e9, etc)
      let unit = Math.floor(((new_this).toFixed(0).length - 1) / 3) * 3;
      // Calculate the remainder
      var num = (new_this / ('1e'+unit)).toFixed(2);
      var unitname = units[Math.floor(unit / 3) - 1];
      
      // output number remainder + unitname
      new_this = num + unitname;
    }
    
    if(sign_change==1) {
      // return formatted original number
      var out = "-$" + new_this.toLocaleString();
      return out
    }  else {
      // return formatted original number
      var out = "$" + new_this.toLocaleString();
      return  out
    }
    
    
  }
  
  // Add method to prototype. this allows you to use this function on numbers and strings directly
  Number.prototype.commarize = commarize;
  String.prototype.commarize = commarize;
  
  var myLanguage = "${e://Field/Q_Language}";
  var myCountry = "${e://Field/country}";
  
  if(myLanguage=='EN') {
  
  var own = ['100% Foreign-owned',
             'Domestic - Private',
             'Domestic - Public'];
  var country = ['United States','Germany','China','Russia','Brazil','South Korea','Japan','Saudi Arabia'];
  var sector = ['Construction',"Manufacturing","Agriculture","Telecommunications","Retail","Energy Sector","Financial Services"];
  
  // sample first top-level treatment
  // then sample specific language
  // top-level treatments:
    // 1. No political connections (control)
  // 2. Military
  // 3. Police
  // 4. MP
  // 5. Government/bureaucracy
  // 6. Personal Relationship
  var connections1 = ["Member of parliament is on board of company",
                      "Member of parliament is on board of company",
                      "Member of parliament is on board of company",
                      "Member of parliament is on board of company",
                      "Head of ministry is on board of company",
                      "Head of ministry is on board of company",
                      "Head of ministry is on board of company",
                      "Head of ministry is on board of company",
                      "Mid-level bureaucrat is on board of company",
                      "Mid-level bureaucrat is on board of company",
                      "Mid-level bureaucrat is on board of company",
                      "Owner is former member of parliament",
                      "Owner is former member of parliament",
                      "Owner is former member of parliament",
                      "Owner is a nephew of the prime minister",
                      "Owner is a niece of the prime minister",
                      "Owner is former general in the military",
                      "Owner is former general in the military",
                      "Owner is former general in the military",
                      "Owner is former general in the military",
                      "Owner is former officer in the police",
                      "Owner is former officer in the police",
                      "Owner is former officer in the police",
                      "Owner is former classmate of the President",
                      "Owner is member of the President's political party",
                      "Owner is member of the President's political party",
                      "Owner is member of the President's political party",
                      "Owner is member of the President's political party",
                      "Owner is member of the President's political party",
                      "Owner is member of the President's political party",
                      "Owner is member of the President's political party",
                      "Owner is married to the President's daughter",
                      "Owner is married to the President's son",
                      "President's son is on the board of the company",
                      "President's daughter is on the board of the company",
                      "Owner has no interest in politics",
                      "Owner has no interest in politics",
                      "Owner has no interest in politics",
                      "Owner has no interest in politics",
                      "Owner has no interest in politics",
                      "Owner has no interest in politics",
                      "Owner has no interest in politics"];
  
  var connections2 = ["Member of parliament is on board of company",
                      "Head of ministry is on board of company",
                      "Mid-level bureaucrat is on board of company",
                      "President's son is on the board of the company",
                      "President's daughter is on the board of the company",
                      "Owner has no interest in politics"];
  
  
} else if(myLanguage=="RU") {
  var own = ['100% иностранное предприятие',
             'отечественные частные фирмы',
             'отечественные государственные фирмы'];
  var country = ['США','Германия','Китай','Россия','Бразилия','Южная Корея','Япония','Саудовская Аравия'];
  var sector = ['Строительство',"Производство","Сельское хозяйство","Телекоммуникации","Ритейл","Энергетика","Финансовые услуги"];
  
  var connections1 = ["Член парламента входит в совет директоров компании ",
                      "Член парламента входит в совет директоров компании ",
                      "Член парламента входит в совет директоров компании ",
                      "Член парламента входит в совет директоров компании ",
                      "Министр входит в совет директоров компании",
                      "Министр входит в совет директоров компании",
                      "Министр входит в совет директоров компании",
                      "Министр входит в совет директоров компании",
                      "Чиновник среднего уровня входит в совет директоров компании",
                      "Чиновник среднего уровня входит в совет директоров компании",
                      "Чиновник среднего уровня входит в совет директоров компании",
                      "Владелец - бывший член парламента",
                      "Владелец - бывший член парламента",
                      "Владелец - бывший член парламента",
                      "Владелец - племянник премьер министра",
                      "Владелец - племянница премьер министра",
                      "Владелец - одноклассник президента",
                      "Владелец - член политической партии президента",
                      "Владелец - член политической партии президента",
                      "Владелец - член политической партии президента",
                      "Владелец - член политической партии президента",
                      "Владелец - член политической партии президента",
                      "Владелец - член политической партии президента",
                      "Владелец - член политической партии президента",
                      "Владелец - зять президента",
                      "Владелица - невестка президента",
                      "Сын президента входит в совет директоров компании",
                      "Дочь президента входит в совет директоров компании",
                      "Владелец - бывший генерал",
                      "Владелец - бывший генерал",
                      "Владелец - бывший генерал",
                      "Владелец - бывший генерал",
                      "Владелец - бывший офицер полиции",
                      "Владелец - бывший офицер полиции",
                      "Владелец - бывший офицер полиции",
                      "У владелеца нет интереса к политике",
                      "У владелеца нет интереса к политике",
                      "У владелеца нет интереса к политике",
                      "У владелеца нет интереса к политике",
                      "У владелеца нет интереса к политике",
                      "У владелеца нет интереса к политике",
                      "У владелеца нет интереса к политике"];
  
  var connections2 = ["Член парламента входит в совет директоров компании ",
                      "Член парламента входит в совет директоров компании ",
                      "Член парламента входит в совет директоров компании ",
                      "Член парламента входит в совет директоров компании ",
                      "Министр входит в совет директоров компании",
                      "Министр входит в совет директоров компании",
                      "Министр входит в совет директоров компании",
                      "Министр входит в совет директоров компании",
                      "Чиновник среднего уровня входит в совет директоров компании",
                      "Чиновник среднего уровня входит в совет директоров компании",
                      "Чиновник среднего уровня входит в совет директоров компании",
                      "Владелец - бывший член парламента",
                      "Владелец - бывший член парламента",
                      "Владелец - бывший член парламента",
                      "Владелец - племянник премьер министра",
                      "Владелец - племянница премьер министра",
                      "Владелец - одноклассник президента",
                      "Владелец - член политической партии президента",
                      "Владелец - член политической партии президента",
                      "Владелец - член политической партии президента",
                      "Владелец - член политической партии президента",
                      "Владелец - член политической партии президента",
                      "Владелец - член политической партии президента",
                      "Владелец - член политической партии президента",
                      "Владелец - зять президента",
                      "Владелица - невестка президента",
                      "Сын президента входит в совет директоров компании",
                      "Дочь президента входит в совет директоров компании",
                      "Владелец - бывший генерал",
                      "Владелец - бывший генерал",
                      "Владелец - бывший генерал",
                      "Владелец - бывший генерал",
                      "Владелец - бывший офицер полиции",
                      "Владелец - бывший офицер полиции",
                      "Владелец - бывший офицер полиции",
                      "У владелеца нет интереса к политике",
                      "У владелеца нет интереса к политике",
                      "У владелеца нет интереса к политике",
                      "У владелеца нет интереса к политике",
                      "У владелеца нет интереса к политике",
                      "У владелеца нет интереса к политике",
                      "У владелеца нет интереса к политике"];
  
} else if(myLanguage=="ES") {
  var own = ['100% empresa extranjera',
             'Nacional - Privada',
             'Nacional - Pública'];
  var country = ['Estados Unidos','Alemania','China','Rusia','Brasil','Corea del Sur','Japón','Arabia Saudita'];
  var sector = ['Construcción',"Manufactura","Agricultura","Telecomunicaciones","Ventas al menudeo","Sector energía","Servicios financieros"];
  
  var connections1 = ["Un miembro del parlamento está en la Junta Directiva de la empresa",
                  "Un miembro del parlamento está en la Junta Directiva de la empresa",
                  "Un miembro del parlamento está en la Junta Directiva de la empresa",
                  "Un miembro del parlamento está en la Junta Directiva de la empresa",
                      "Un ministro está en la Junta Directiva de la empresa",
                      "Un ministro está en la Junta Directiva de la empresa",
                      "Un ministro está en la Junta Directiva de la empresa",
                      "Un ministro está en la Junta Directiva de la empresa",
                      "Un funcionario público de rango intermedio está en la Junta Directiva de la empresa",
                      "Un funcionario público de rango intermedio está en la Junta Directiva de la empresa",
                      "Un funcionario público de rango intermedio está en la Junta Directiva de la empresa",
                      "El propietario es un ex miembro del parlamento",
                      "El propietario es un ex miembro del parlamento",
                      "El propietario es un ex miembro del parlamento",
                      "El propietario es sobrino del primer ministro",
                      "La propietaria es sobrina del primer ministro",
                      "El propietario es un general retirado de las Fuerzas Armadas",
                      "El propietario es un general retirado de las Fuerzas Armadas",
                      "El propietario es un general retirado de las Fuerzas Armadas",
                      "El propietario es un general retirado de las Fuerzas Armadas",
                      "El propietario es ex oficial de policía",
                      "El propietario es ex oficial de policía",
                      "El propietario es ex oficial de policía",
                      "El propietario es ex compañero de clase del presidente",
                      "El propietario no tiene interés en la política",
                      "El propietario no tiene interés en la política",
                      "El propietario no tiene interés en la política",
                      "El propietario no tiene interés en la política",
                      "El propietario no tiene interés en la política",
                      "El propietario no tiene interés en la política",
                      "El propietario no tiene interés en la política",
                      "El propietario es miembro del partido político del presidente",
                      "El propietario es miembro del partido político del presidente",
                      "El propietario es miembro del partido político del presidente",
                      "El propietario es miembro del partido político del presidente",
                      "El propietario es miembro del partido político del presidente",
                      "El propietario es miembro del partido político del presidente",
                      "El propietario es miembro del partido político del presidente",
                      "El propietario está casado con un hijo del presidente",
                      "La propietaria está casado con una hija del presidente",
                      "Un hijo del presidente forma parte de la Junta Directiva de la empresa",
                      "Una hija del presidente forma parte de la Junta Directiva de la empresa"];
  
  var connections2 = ["Un miembro del parlamento está en la Junta Directiva de la empresa",
                  "Un miembro del parlamento está en la Junta Directiva de la empresa",
                  "Un miembro del parlamento está en la Junta Directiva de la empresa",
                  "Un miembro del parlamento está en la Junta Directiva de la empresa",
                      "Un ministro está en la Junta Directiva de la empresa",
                      "Un ministro está en la Junta Directiva de la empresa",
                      "Un ministro está en la Junta Directiva de la empresa",
                      "Un ministro está en la Junta Directiva de la empresa",
                      "Un funcionario público de rango intermedio está en la Junta Directiva de la empresa",
                      "Un funcionario público de rango intermedio está en la Junta Directiva de la empresa",
                      "Un funcionario público de rango intermedio está en la Junta Directiva de la empresa",
                      "El propietario es un ex miembro del parlamento",
                      "El propietario es un ex miembro del parlamento",
                      "El propietario es un ex miembro del parlamento",
                      "El propietario es sobrino del primer ministro",
                      "La propietaria es sobrina del primer ministro",
                      "El propietario es un general retirado de las Fuerzas Armadas",
                      "El propietario es un general retirado de las Fuerzas Armadas",
                      "El propietario es un general retirado de las Fuerzas Armadas",
                      "El propietario es un general retirado de las Fuerzas Armadas",
                      "El propietario es ex oficial de policía",
                      "El propietario es ex oficial de policía",
                      "El propietario es ex oficial de policía",
                      "El propietario es ex compañero de clase del presidente",
                      "El propietario no tiene interés en la política",
                      "El propietario no tiene interés en la política",
                      "El propietario no tiene interés en la política",
                      "El propietario no tiene interés en la política",
                      "El propietario no tiene interés en la política",
                      "El propietario no tiene interés en la política",
                      "El propietario no tiene interés en la política",
                      "El propietario es miembro del partido político del presidente",
                      "El propietario es miembro del partido político del presidente",
                      "El propietario es miembro del partido político del presidente",
                      "El propietario es miembro del partido político del presidente",
                      "El propietario es miembro del partido político del presidente",
                      "El propietario es miembro del partido político del presidente",
                      "El propietario es miembro del partido político del presidente",
                      "El propietario está casado con un hijo del presidente",
                      "La propietaria está casado con una hija del presidente",
                      "Un hijo del presidente forma parte de la Junta Directiva de la empresa",
                      "Una hija del presidente forma parte de la Junta Directiva de la empresa"];
} else if(myLanguage=="AR") {
      var connections1 = ["يتضمن مجلس إدارة الشركة عضو من البرلمان",
      "يتضمن مجلس إدارة الشركة عضو من البرلمان",
      "يتضمن مجلس إدارة الشركة عضو من البرلمان",
      "يتضمن مجلس إدارة الشركة عضو من البرلمان",
                        " يتضمن مجلس إدارة الشركة رئيس الوزارة",
                        " يتضمن مجلس إدارة الشركة رئيس الوزارة",
                        " يتضمن مجلس إدارة الشركة رئيس الوزارة",
                        " يتضمن مجلس إدارة الشركة رئيس الوزارة",
                        " يتضمن مجلس إدارة الشركة بيروقراطي متوسط المستوى",
                        " يتضمن مجلس إدارة الشركة بيروقراطي متوسط المستوى",
                        " يتضمن مجلس إدارة الشركة بيروقراطي متوسط المستوى",
                        " المالك عضو سابق في البرلمان",
                        " المالك عضو سابق في البرلمان",
                        " المالك عضو سابق في البرلمان",
                        " المالك هو إبن أخ رئيس الوزراء",
                        " المالكة هي بنت أخ رئيس الوزراء",
                        " المالك  جنرال سابق في الجيش",
                        " المالك  جنرال سابق في الجيش",
                        " المالك  جنرال سابق في الجيش",
                        " المالك  جنرال سابق في الجيش",
                        " المالك  ضابط سابق في الشرطة",
                        " المالك  ضابط سابق في الشرطة",
                        " المالك  ضابط سابق في الشرطة",
                        " المالك  زميل دراسة سابق للرئيس",
                        " المالك عضو في الحزب السياسي الذي ينتمي له الرئيس",
                        " المالك عضو في الحزب السياسي الذي ينتمي له الرئيس",
                        " المالك عضو في الحزب السياسي الذي ينتمي له الرئيس",
                        " المالك عضو في الحزب السياسي الذي ينتمي له الرئيس",
                        " المالك عضو في الحزب السياسي الذي ينتمي له الرئيس",
                        " المالك عضو في الحزب السياسي الذي ينتمي له الرئيس",
                        " المالك عضو في الحزب السياسي الذي ينتمي له الرئيس",
                        " المالك متزوج من ابنة الرئيس",
                        " المالكة متزوجة من ابن الرئيس",
                        " يتضمن مجلس إدارة الشركة إبن الرئيس",
                        " يتضمن مجلس إدارة الشركة إبنة الرئيس",
                        " المالك غير مهتم بالسياسية",
                        " المالك غير مهتم بالسياسية",
                        " المالك غير مهتم بالسياسية",
                        " المالك غير مهتم بالسياسية",
                        " المالك غير مهتم بالسياسية",
                        " المالك غير مهتم بالسياسية",
                        " المالك غير مهتم بالسياسية"];
    
    var connections2 = ["يتضمن مجلس إدارة الشركة عضو من البرلمان",
                        " يتضمن مجلس إدارة الشركة رئيس الوزارة",
                        " يتضمن مجلس إدارة الشركة بيروقراطي متوسط المستوى",
                        " يتضمن مجلس إدارة الشركة إبن الرئيس",
                        " يتضمن مجلس إدارة الشركة إبنة الرئيس",
                        " المالك غير مهتم بالسياسية"];

    var own = ['100% ملكية أجنبية',
               'محلي - خاص',
               'محلي - عام'];
    var country = ['الولايات المتحدة الأمريكية','ألمانيا','الصين','روسيا','البرازيل','كوريا الجنوبية','اليابان','المملكة العربية السعودية'];
    var sector = ['الإنشاءات',"التصنيع","الزراعة","الإتصالات","بيع التجزئة","قطاع الطاقة","خدمات التمويل"];

}

  // randomize numeric twice
  
  // get exogenous growth shock
  
  var growth_shock = [0];
  
  for(t=1;t<11;t++) {
    
    
  }
  
  // choose capital/labor returns to scale
  
  var capreturn = .5;
  var labreturn = .5;
  
  Qualtrics.SurveyEngine.setEmbeddedData('capreturn',capreturn);
  Qualtrics.SurveyEngine.setEmbeddedData('labreturn',labreturn);
  
  // sample employees from one of several distributions
  var empl_rand = Math.random();
  if(empl_rand<.2) {
    
    var employees = Math.floor(10 + Math.random()*40);
    
  } else if(empl_rand<.4) {
    var employees = Math.floor(50 + Math.random()*50);
  } else if(empl_rand<.6) {
    var employees = Math.floor(100 + Math.random()*400);
  } else if(empl_rand<.8) {
    var employees = Math.floor(500 + Math.random()*500);
  } else {
    var employees = Math.floor(1000 + Math.random()*4000);
  }
  var assets = Math.floor(employees * (100 + Math.random()*10000)); // fixed conditional on number of employees
  // measures how capital-intensive firm is
   var  tfp = .25 - Math.random()*.5; // TFP can be greater or lower than 1 = TFP intercept
  var tfp_cov1 = 0.25 - Math.random()*.5; // TFP linear predictor
  var tfp_cov2 = 0.01 - Math.random()*.02; // TFP quandratic predictor
  // we assume that assets and employees are relatively constant over time,
  // but tfp is going to vary
  // here we set the initial conditions year = 1
   // profit = TFP greater or lower than 1 versus firm
  var years = 3 + Math.floor(Math.random()*65);
  
  // need to draw a sigma parameter to reflect the overall level of variance
  
  console.log(tfp_cov1);
  var sales_time = [];
  var profit_time = [];
  var tfp_time = []; // holding the time-varying TFP values
  
  // first generate time-varying TFP
  for(i=0; i < (years-1); i++) {
    // quadratic function in the reals
      tfp_time[i] =  tfp + tfp_cov1*i + tfp_cov2*i*i;
  }
  
  var tfp_std = jStat.stdev(tfp_time); // need to standardize to make sure it goesn't too big
	
	for(i=0; i < (years-1); i++) {
  	tfp_time[i] = -1 * (tfp_time[i]/tfp_std);
  }
	
  tfp_time = jStat(tfp_time).exp()[0];
  
  // standardize and use logit function
  // calculate profits and sales given TFP (no errors)
  for(i=0;  i < (years-1); i++) {
    tfp_time[i] = 2*(1/(1 + tfp_time[i]));
     sales_time[i] = Math.pow(assets,capreturn)*Math.pow(employees*5000,labreturn)*tfp_time[i];
    profit_time[i] = sales_time[i] -Math.pow(assets,capreturn)*Math.pow(employees*5000,labreturn);
  }
  
  var this_own = own[Math.floor(Math.random()*own.length)];
  
  // need to adjust assignment for foreign vs. not foreign firms
  
  var foreign_list = ["100% Foreign-owned",
                      '100% иностранное предприятие',
                      '100% empresa extranjera'];
  
  
  if(foreign_list.includes(this_own)) {
    var this_country = 	country[Math.floor(Math.random()*country.length)];
    var connections = connections2;
  } else {
    var this_country = myCountry;
    var connections = connections1;
  }
  
  
  Qualtrics.SurveyEngine.setEmbeddedData('ownA',this_own);
  Qualtrics.SurveyEngine.setEmbeddedData('countryA',this_country);
  Qualtrics.SurveyEngine.setEmbeddedData('sectorA',sector[Math.floor(Math.random()*sector.length)]);
  Qualtrics.SurveyEngine.setEmbeddedData('employeesA',employees);
  Qualtrics.SurveyEngine.setEmbeddedData('profitA',profit_time);
  Qualtrics.SurveyEngine.setEmbeddedData('salesA',sales_time);
  //Qualtrics.SurveyEngine.setEmbeddedData('salessigmaA',sigma_sales);
  //Qualtrics.SurveyEngine.setEmbeddedData('profitsigmaA',sigma_profit);
  Qualtrics.SurveyEngine.setEmbeddedData('tfpA',tfp_time);
  Qualtrics.SurveyEngine.setEmbeddedData('assetsA', assets.commarize());
  Qualtrics.SurveyEngine.setEmbeddedData('yearsA',years);
  Qualtrics.SurveyEngine.setEmbeddedData('connectionsA',connections[Math.floor(Math.random()*connections.length)]);
  
  // sample employees from one of several distributions
  empl_rand = Math.random();
  if(empl_rand<.2) {
    
    employees = Math.floor(10 + Math.random()*40);
    
  } else if(empl_rand<.4) {
    employees = Math.floor(50 + Math.random()*50);
  } else if(empl_rand<.6) {
    employees = Math.floor(100 + Math.random()*400);
  } else if(empl_rand<.8) {
    employees = Math.floor(500 + Math.random()*500);
  } else {
    employees = Math.floor(1000 + Math.random()*4000);
  }
  assets = Math.floor(employees * (100 + Math.random()*10000)); // fixed conditional on number of employees
  // measures how capital-intensive firm is
  tfp = .25 - Math.random()*.5; // TFP can be greater or lower than 1 = TFP intercept
  tfp_cov1 = 0.25 - Math.random()*.5; // TFP linear predictor
  tfp_cov2 = 0.01 - Math.random()*.02; // TFP quandratic predictor
  
  this_own = own[Math.floor(Math.random()*own.length)];
  
  // we assume that assets and employees are relatively constant over time,
  // but tfp is going to vary
 // profit = TFP greater or lower than 1 versus firm
  years = 3 + Math.floor(Math.random()*65);
  
  sales_time = [];
  profit_time = [];
  tfp_time = []; // holding the time-varying TFP values
  
  // first generate time-varying TFP
  for(i=0; i < (years-1); i++) {
    // quadratic function in the reals
      tfp_time[i] =  tfp + tfp_cov1*i + tfp_cov2*i*i;
  }
  
  var tfp_std = jStat.stdev(tfp_time); // need to standardize to make sure it goesn't too big
	
	for(i=0; i < (years-1); i++) {
  	tfp_time[i] = -1 * (tfp_time[i]/tfp_std);
  }
	
  tfp_time = jStat(tfp_time).exp()[0];
  
  // standardize and use logit function
  // calculate profits and sales given TFP (no errors)
  for(i=0;  i < (years-1); i++) {
    tfp_time[i] = 2*(1/(1 + tfp_time[i]));
    sales_time[i] = Math.pow(assets,capreturn)*Math.pow(employees*5000,labreturn)*tfp_time[i];
    profit_time[i] = sales_time[i] -Math.pow(assets,capreturn)*Math.pow(employees*5000,labreturn);
  }
  
  if(foreign_list.includes(this_own)) {
    var this_country = 	country[Math.floor(Math.random()*country.length)];
    var connections = connections2;
  } else {
    var this_country = myCountry;
    var connections = connections1;
  }
  
  Qualtrics.SurveyEngine.setEmbeddedData('ownB',this_own);
  Qualtrics.SurveyEngine.setEmbeddedData('countryB',this_country);
  Qualtrics.SurveyEngine.setEmbeddedData('sectorB',sector[Math.floor(Math.random()*sector.length)]);
  Qualtrics.SurveyEngine.setEmbeddedData('employeesB',employees);
  Qualtrics.SurveyEngine.setEmbeddedData('profitB',profit_time);
  //Qualtrics.SurveyEngine.setEmbeddedData('salessigmaB',sigma_sales);
  //Qualtrics.SurveyEngine.setEmbeddedData('profitsigmaB',sigma_profit);
  Qualtrics.SurveyEngine.setEmbeddedData('salesB',sales_time);
  Qualtrics.SurveyEngine.setEmbeddedData('tfpB',tfp_time);
  Qualtrics.SurveyEngine.setEmbeddedData('assetsB',assets.commarize());
  Qualtrics.SurveyEngine.setEmbeddedData('yearsB',years);
  Qualtrics.SurveyEngine.setEmbeddedData('connectionsB',connections[Math.floor(Math.random()*connections.length)]);
  
  
});

Qualtrics.SurveyEngine.addOnReady(function()
{
  /*Place your JavaScript here to run when the page is fully displayed*/
    
});

Qualtrics.SurveyEngine.addOnUnload(function()
{
  /*Place your JavaScript here to run when the page is unloaded*/
    
});