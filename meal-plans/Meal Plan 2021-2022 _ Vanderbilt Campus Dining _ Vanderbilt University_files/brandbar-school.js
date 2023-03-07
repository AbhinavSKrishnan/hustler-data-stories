// NEW OMNI templates are having the CSEngine var declared in the meta.xsl file
// NEW WORDPRESS templates have this var declared in header.php
// FOR TESTING
// var CSEngine = 'https://news.vanderbilt.edu/search';

var CSEngine;
var VUWPurl;
var head = document.head || document.getElementsByTagName("head")[0];

var vuStyle = document.createElement("link");
vuStyle.type = "text/css";
vuStyle.rel = "stylesheet";
vuStyle.href = "https://cdn.vanderbilt.edu/vu-www4/brandbar/css/vu-brandbar.css";

head.appendChild(vuStyle);

var vuitLink = document.createElement("a");
vuitLink.href = "https://ibqpinew3g.execute-api.us-east-1.amazonaws.com/ProdStage";
vuitLink.style = "display: none;";
vuitLink.rel = "nofollow";
vuitLink.setAttribute("aria-hidden", "true");
vuitLink.setAttribute("aria-label", "VUIT-Link");
vuitLink.innerHTML = "<!-- VUIT Link -->";

document.body.appendChild(vuitLink);

// what logo files should we use
if (typeof BrandbarSchool !== 'undefined' && BrandbarSchool && BrandbarSchool !== 'Vanderbilt') {
    //var VUSchoolLogo = 'https://cdn.vanderbilt.edu/vu-www4/brandbar/images/'+BrandbarSchool+'.png';
    var VUSchoolLogo = 'https://cdn.vanderbilt.edu/vu-www4/brandbar/svg/Optimized/'+BrandbarSchool+'.svg';
    switch (BrandbarSchool) {
        case 'Engineering':
            var schoolLink = 'https://engineering.vanderbilt.edu';
            break;
        case 'Peabody':
            var schoolLink = 'https://peabody.vanderbilt.edu';
            break;
        case 'Blair':
            var schoolLink = 'https://blair.vanderbilt.edu';
            break;
        case 'Nursing':
            var schoolLink = 'https://nursing.vanderbilt.edu';
            break;
        case 'Divinity':
            var schoolLink = 'https://divinity.vanderbilt.edu';
            break;
        case 'CAS':
            var schoolLink = 'https://as.vanderbilt.edu';
            break;
        case 'Graduate':
            var schoolLink = 'https://gradschool.vanderbilt.edu';
            break;
        case 'Law':
            var schoolLink = 'https://law.vanderbilt.edu';
            break;
        case 'Owen':
            var schoolLink = 'https://business.vanderbilt.edu';
            break;
        case 'Medicine':
            var schoolLink = 'https://medschool.vanderbilt.edu';
            break;
        default:
            var schoolLink = 'https://www.vanderbilt.edu';
    }
} else {
    //var VUSchoolLogo = 'https://cdn.vanderbilt.edu/vu-www4/brandbar/images/Vanderbilt.png';
    var VUSchoolLogo = 'https://cdn.vanderbilt.edu/vu-www4/brandbar/svg/Optimized/vanderbilt.svg';
    var schoolLink = 'https://www.vanderbilt.edu';
}

/////////////////////////
// USING WORDPRESS SEARCH
/////////////////////////
if (typeof searchMethod !== 'undefined' && searchMethod === 'Wordpress Search') {
    if (typeof GSAsitesearch !== 'undefined') {
        var VUWPurl = GSAsitesearch;
    } else if (typeof VUWPurl === 'undefined') {
        var VUWPurl = schoolLink + '/search';
    }
    var CleanSiteURL = VUWPurl
    .replace(/^https\:\/\//, 'SSL').replace(/^http\:\/\//, 'NONSSL')
    // remove the leading protocols (temporarily)
    .replace(/\/+/g, '/')       // replace consecutive slashes with a single slash
    .replace(/\/+$/, '')         // remove trailing slashes
    .replace('SSL', 'https://').replace('NONSSL', 'http://');  // add back in protocol

    var SearchVariable = 's';
    var SearchAction = CleanSiteURL;
}
/////////////////////////
// USING GOOGLE CSE SEARCH
/////////////////////////
else {
    var SearchVariable = 'q';
    if (CSEngine) {
        var SearchAction = CSEngine;
    } else {
        var SearchAction = schoolLink + '/search';
    }
}

/////////////////////////
// SEARCH FORM
/////////////////////////
var searchForm = '<form action="'+SearchAction+'" method="get" id="VanderbiltSearch" class="navbar-form-expanded navbar-form navbar-left visible-lg-block visible-md-block visible-xs-block" role="search">'+
    '    <div class="vu-toolbar__group input-group">'+
    '    <input type="text" class="form-control" data-width="80px" data-width-expanded="170px" placeholder="Search..." name="'+SearchVariable+'" aria-label="text to search for">'+
    '    <span class="vu-toolbar__group-btn input-group-btn"><button class="btn btn-default" aria-label="submit search" type="submit" name="submit"><i class="vubrandbar__icon glyphicon glyphicon-search"></i>&nbsp;</button>'+
    '    </span>'+
    '    </div>'+
    '    </form>';

//additional content for brandbar
var additionalContent = '';

if (typeof BrandbarSchool !== 'undefined' && BrandbarSchool && BrandbarSchool === 'Medicine') {
    additionalContent = '<!-- VU SOM -->' +
    '            <li class="dropdown">' +
    '                <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false"><i class="vubrandbar__icon glyphicon glyphicon-th"></i>&nbsp; Your VUSM <span class="caret"></span></a>' +
    '                <ul class="dropdown-menu">' +

    '                    <li><a href="https://medschool.vanderbilt.edu/explore-vusm/">Current Students</a></li>' +
    '                    <li><a href="https://medschool.vanderbilt.edu/postdoc/">Postdocs</a></li>' +
    '                    <li><a href="https://www.vumc.org/gme/">Residents</a></li>' +
    '                    <li><a href="https://medschool.vanderbilt.edu/basic-sciences/faculty">Basic Sciences Faculty Affairs</a></li>' +
    '                    <li><a href="https://www.vumc.org/faculty/">Clinical Faculty Affairs</a></li>' +
    '                    <li><a href="https://medschool.vanderbilt.edu/alumni/">Alumni</a></li>' +
    '                    <li><a href="https://www.vanderbilthealth.com/">Patients</a></li>' +
    '                    <li><a href="http://vu.edu/peoplefinder">People Finder</a></li>' +
    '                    <li><a href="https://medschool.vanderbilt.edu/a-z-directory/">A-Z Directory</a></li>' +
    '                    <li><a href="https://www.vanderbilt.edu/catalogs/kuali/som-22-23.php#/home" target="_blank" rel="noopener">VUSM Catalog</a></li>' +
    '                    <li><a href="https://medschool.vanderbilt.edu/">School of Medicine</a></li>' +
    '                    <li><a href="https://www.vanderbilt.edu/">Vanderbilt University</a></li>' +
    '                    <li><a href="https://www.vumc.org/">Vanderbilt University Medical Center</a></li>' +
    '                </ul>' +
    '            </li>';
} else if (typeof BrandbarSchool !== 'undefined' && BrandbarSchool && BrandbarSchool === 'Peabody') {
    additionalContent = '<!-- VU Peabody -->' +
  '            <!-- Apply -->' +
  '            <li><a href="/degrees-programs/masters-edd-programs/" role="button" aria-haspopup="true" aria-expanded="false"><i class="vubrandbar__icon fa fa-pencil-square-o"></i>&nbsp; Apply</a></li>' +
  '            <!-- Your Peabody -->' +
  '            <li class="dropdown">' +
  '                <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false"><i class="vubrandbar__icon glyphicon glyphicon-th"></i>&nbsp; Your Peabody <span class="caret"></span></a>' +
  '                <ul class="dropdown-menu">' +
  '                    <li><a href="https://peabody.vanderbilt.edu/about/greatergood">Prospective Students</a></li>' +
  '                    <li><a href="https://peabody.vanderbilt.edu/your-peabody/current-students.php">Current Students</a></li>' +
  '                    <li><a href="https://peabody.vanderbilt.edu/your-peabody/faculty-staff.php">Faculty/Staff</a></li>' +
  '                    <li><a href="https://peabody.vanderbilt.edu/alumni/">Alumni</a></li>' +
  '                    <li><a href="https://peabody.vanderbilt.edu/admin-offices/career-development/for_employers.php">Employers</a></li>' +
  '                    <li><a href="https://news.vanderbilt.edu/resources/">Media</a></li>' +
  '                </ul>' +
  '            </li>';
} else if (typeof BrandbarSchool !== 'undefined' && BrandbarSchool && BrandbarSchool === 'CAS') {
    additionalContent = '<!-- VU College of Arts & Science -->'+
    '            <!-- Apply -->'+
    '            <li><a href="https://admissions.vanderbilt.edu/apply/" role="button" aria-haspopup="true" aria-expanded="false"><i class="vubrandbar__icon fa fa-pencil-square-o"></i>&nbsp; Apply</a></li>'+
    '            <!-- Explore VU -->'+
    '            <li class="dropdown">'+
    '                <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false"><i class="vubrandbar__icon glyphicon glyphicon-th"></i>&nbsp; Explore VU <span class="caret"></span></a>'+
    '                <ul class="dropdown-menu">'+
    '                    <li><a href="https://www.vanderbilt.edu/">Vanderbilt Home</a></li>'+
    '                    <li><a href="https://www.vanderbilt.edu/about/">About</a></li>'+
    '                    <li><a href="https://www.vanderbilt.edu/prospective/">Admissions</a></li>'+
    '                    <li><a href="https://www.vanderbilt.edu/academics/">Academics</a></li>'+
    '                    <li><a href="https://research.vanderbilt.edu/">Research</a></li>'+
    '                    <li><a href="https://www.vanderbilt.edu/student/">Students</a></li>'+
    '                    <li><a href="https://www.vanderbilt.edu/faculty-staff/">Faculty &amp; Staff</a></li>'+
    '                    <li><a href="https://www.vucommodores.com/">Athletics</a></li>'+
    '                    <li><a href="https://news.vanderbilt.edu/">News &amp; Events</a></li>'+
    '                    <li class="last"><a href="https://social.vanderbilt.edu/">Get Social @Vanderbilt</a></li>'+
    '                </ul>'+
    '            </li>';
} else {
    additionalContent = '<!-- Explore VU -->'+
    '            <li class="dropdown">'+
    '                <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false"><i class="vubrandbar__icon glyphicon glyphicon-th"></i>&nbsp; Explore VU <span class="caret"></span></a>'+
    '                <ul class="dropdown-menu">'+
    '                    <li><a href="https://www.vanderbilt.edu/">Vanderbilt Home</a></li>'+
    '                    <li><a href="https://www.vanderbilt.edu/about/">About</a></li>'+
    '                    <li><a href="https://www.vanderbilt.edu/prospective/">Admissions</a></li>'+
    '                    <li><a href="https://www.vanderbilt.edu/academics/">Academics</a></li>'+
    '                    <li><a href="https://research.vanderbilt.edu/">Research</a></li>'+
    '                    <li><a href="https://www.vanderbilt.edu/student/">Students</a></li>'+
    '                    <li><a href="https://www.vanderbilt.edu/faculty-staff/">Faculty &amp; Staff</a></li>'+
    '                    <li><a href="https://www.vucommodores.com/">Athletics</a></li>'+
    '                    <li><a href="https://news.vanderbilt.edu/">News &amp; Events</a></li>'+
    '                    <li class="last"><a href="https://social.vanderbilt.edu/">Get Social @Vanderbilt</a></li>'+
    '                </ul>'+
    '            </li>';
}

// start building the brandbar
var vuTopBar = '<div class="visible-print-block text-center">'+
    '   <img src="https://cdn.vanderbilt.edu/vu-www4/omni/i/vu-logo-print.png" alt="Vanderbilt University Logo" height="34px" />'+
    '</div>'+
    '<nav class="navbar navbar-inverse" id="main_navbar" role="navigation">' +
    '<div class="container">'+
    '    <div class="navbar-header">'+
    '        <button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse">'+
    '        <span class="sr-only">Toggle navigation</span>'+
    '        <span class="icon-bar"></span>'+
    '        <span class="icon-bar"></span>'+
    '        <span class="icon-bar"></span>'+
    '        </button>'+
    '        <a class="navbar-brand" href="'+schoolLink+'">'+
    '            <img alt="Vanderbilt University | '+BrandbarSchool+'" class="hidden-xs hidden-sm hidden-md" width="100%" height="90%" style="max-width:280px;width: 280px;" src="'+VUSchoolLogo+'" />'+
    '        <img class="visible-xs visible-sm visible-md" width="100%" height="90%" src="https://cdn.vanderbilt.edu/vu-www4/brandbar/svg/Optimized/vanderbilt.svg" alt="Vanderbilt University | '+BrandbarSchool+'" /></a>'+
    '    </div>'+
    '    <div class="vubrandbar collapse navbar-collapse">'+
    '        <ul class="nav navbar-nav navbar-right vu-toolbar">'+
    '            <li>'+
    '               <!-- search form -->'+
                    searchForm+
    '            </li>'+
                    additionalContent+
    '        </ul>'+
    '    </div>'+
    '</div>'+
    '</nav>';

document.body.insertAdjacentHTML('afterbegin', vuTopBar);

var vuEmergency = document.createElement("script");
vuEmergency.type = "text/javascript";
vuEmergency.src = "https://cdn.vanderbilt.edu/vu-www4/brandbar/emergency.js";

document.body.appendChild(vuEmergency);