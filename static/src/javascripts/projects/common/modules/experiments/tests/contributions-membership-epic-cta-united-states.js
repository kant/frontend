define([
    'bean',
    'qwery',
    'common/utils/$',
    'common/utils/template',
    'common/views/svg',
    'common/utils/fastdom-promise',
    'common/utils/mediator',
    'text!common/views/contributions-epic-equal-buttons.html',
    'text!common/views/contributions-epic.html',
    'common/utils/robust',
    'inlineSvg!svgs/icon/arrow-right',
    'common/utils/config',
    'common/utils/cookies',
    'common/modules/experiments/embed',
    'common/utils/ajax',
    'common/modules/commercial/commercial-features',
    'lodash/arrays/intersection'

], function (bean,
             qwery,
             $,
             template,
             svg,
             fastdom,
             mediator,
             contributionsEpicEqualButtons,
             contributionsEpic,
             robust,
             arrowRight,
             config,
             cookies,
             embed,
             ajax,
             commercialFeatures,
             intersection) {


    return function () {

        this.id = 'ContributionsMembershipEpicCtaUnitedStatesTwo';
        this.start = '2016-11-08';
        this.expiry = '2016-11-11';
        this.author = 'Jonathan Rankin';
        this.description = 'Test 3 different CTA configurations for the Epic in the US';
        this.showForSensitive = false;
        this.audience = 1;
        this.audienceOffset = 0;
        this.successMeasure = 'Impressions to number of contributions/supporter signups';
        this.audienceCriteria = 'All readers in the US reading about US politics OR the US election, as well as not on Brexit articles ';
        this.dataLinkNames = '';
        this.idealOutcome = 'We learn the best way to present contributions and membership asks in Epic component';
        this.canRun = function () {
            var whitelistedKeywordIds = ['us-news/us-elections-2016', 'us-news/us-politics',
                'us-news/us-news', 'world/world', 'politics/politics', 'environment/environment',
                'politics/eu-referendum', 'society/society', 'australia-news/australia-news' ];

            var hasKeywordsMatch = function() {
                var pageKeywords = config.page.keywordIds;
                return pageKeywords && intersection(whitelistedKeywordIds, pageKeywords.split(',')).length > 0;
            };

            var userHasNeverContributed = !cookies.get('gu.contributions.contrib-timestamp');
            var worksWellWithPageTemplate = (config.page.contentType === 'Article') && !config.page.isMinuteArticle; // may render badly on other types
            return userHasNeverContributed && commercialFeatures.canReasonablyAskForMoney && worksWellWithPageTemplate && hasKeywordsMatch();
        };



        var membershipUrl = 'https://membership.theguardian.com/supporter?';
        var contributeUrl = 'https://contribute.theguardian.com/?';


        var message = '...we have a small favor to ask. More people are reading the Guardian than ever but far fewer are paying for it. And advertising revenues across the media are falling fast. So you can see why we need to ask for your' +
            ' help. The Guardian\'s independent, investigative journalism takes a lot of time, money and hard work to produce. But we do it because we believe our perspective matters – because it might well be your ' +
            'perspective, too.';

        var cta = {
            contributionsMain : {
                p2: 'If everyone who reads our reporting, who likes it, helps to pay for it our future would be more secure. You can give money to the Guardian in less than a minute.',
                p3: 'Alternatively, you can join the Guardian and get even closer to our journalism by ',
                cta1: 'Make a contribution',
                cta2: 'becoming a Supporter.'
            },

            membershipMain : {
                p2: 'If everyone who reads our reporting – who believes in it – helps to support it, our future would be more secure. Get closer to our journalism, be part of our story and join the Guardian.',
                p3: 'Alternatively, you can ',
                cta1: 'Become a supporter',
                cta2: 'make a one-off contribution.'
            },

            equal: {
                p2: 'If everyone who reads our reporting – who believes in it – helps to support it, our future would be more secure. Give to the Guardian by becoming a Supporter or by making a one-off contribution.',
                p3: '',
                cta1: 'Become a supporter',
                cta2: 'Make a contribution'
            }
        };

        var componentWriter = function (component) {
            ajax({
                url: 'https://api.nextgen.guardianapps.co.uk/geolocation',
                method: 'GET',
                contentType: 'application/json',
                crossOrigin: true
            }).then(function (resp) {
                if(resp.country === 'US') {
                    fastdom.write(function () {
                        var submetaElement = $('.submeta');
                        if(submetaElement.length > 0) {
                            component.insertBefore(submetaElement);
                            embed.init();
                            mediator.emit('contributions-embed:insert', component);
                        }
                    });
                }
            });
        };

        var makeUrl = function(urlPrefix, intcmp) {
            return urlPrefix + 'INTCMP=' + intcmp;
        };

        var completer = function (complete) {
            mediator.on('contributions-embed:insert', complete);
        };

        var contributeUrlPrefix = 'co_us_epic_footer_';
        var membershipUrlPrefix = 'gdnwb_copts_mem_epic_';

        this.variants = [
            {
                id: 'control',
                test: function () {
                    var component = $.create(template(contributionsEpic, {
                        linkUrl1: makeUrl(contributeUrl, contributeUrlPrefix + 'm1_contributions_main_us_2'),
                        linkUrl2: makeUrl(membershipUrl, membershipUrlPrefix + 'm1_contributions_main_us_2'),
                        p1: message,
                        p2: cta.contributionsMain.p2,
                        p3: cta.contributionsMain.p3,
                        cta1: cta.contributionsMain.cta1,
                        cta2: cta.contributionsMain.cta2,
                        hidden: ''
                    }));
                    componentWriter(component);
                },
                impression: function(track) {
                    mediator.on('contributions-embed:insert', track);
                },
                success: completer
            },
            {
                id: 'membership',
                test: function () {
                    var component = $.create(template(contributionsEpic, {
                        linkUrl1: makeUrl(membershipUrl, membershipUrlPrefix  + 'm1_membership_main_us_2'),
                        linkUrl2: makeUrl(contributeUrl, contributeUrlPrefix + 'm1_membership_main_us_2'),
                        p1: message,
                        p2: cta.membershipMain.p2,
                        p3: cta.membershipMain.p3,
                        cta1: cta.membershipMain.cta1,
                        cta2: cta.membershipMain.cta2,
                        hidden: ''
                    }));
                    componentWriter(component);
                },
                impression: function(track) {
                    mediator.on('contributions-embed:insert', track);
                },
                success: completer
            },
            {
                id: 'equal',
                test: function () {
                    var component = $.create(template(contributionsEpicEqualButtons, {
                        linkUrl1: makeUrl(membershipUrl, membershipUrlPrefix + 'm1_equal_us_2'),
                        linkUrl2: makeUrl(contributeUrl, contributeUrlPrefix + 'm1_equal_us_2'),
                        p1: message,
                        p2: cta.equal.p2,
                        cta1: cta.equal.cta1,
                        cta2: cta.equal.cta2,
                        hidden: ''
                    }));
                    componentWriter(component);
                },
                impression: function(track) {
                    mediator.on('contributions-embed:insert', track);
                },
                success: completer
            }


        ];
    };
});