library(shiny)
library(shinyAce)


shinyUI(bootstrapPage(


    headerPanel("Correspondence Analysis"),

    mainPanel(
        tabsetPanel(

        tabPanel("Main",

            h3("Data"),
            p('Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),
            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Please make sure that your data includes the header (variable names) in the first row.</div></b>")),

            strong('Option:'),
            checkboxInput("rowname", label = strong("The first column contains case names."), value = T),

            aceEditor("text", value="Phrase\tAL\tELT\tLL\tMLJ\tRELC\tSystem\tTESOLQ\tAgriculture\tBiology\tEarthScience\tEngineering\tFoodScience\tMedicine\tPhysics\non the other hand\t319\t204\t578\t374\t222\t336\t378\t60\t123\t197\t317\t36\t472\t230\nin the case of\t261\t124\t523\t463\t111\t232\t170\t85\t152\t249\t371\t76\t325\t365\non the basis of\t180\t89\t526\t412\t114\t225\t246\t54\t112\t238\t185\t41\t452\t89\nat the end of\t136\t218\t207\t579\t136\t222\t264\t77\t165\t97\t139\t35\t346\t59\nas well as the\t116\t92\t196\t477\t101\t142\t187\t56\t129\t151\t209\t33\t316\t143\nthe end of the\t121\t171\t198\t548\t122\t194\t255\t88\t125\t124\t136\t26\t275\t55\nas a function of\t21\t0\t113\t137\t0\t0\t23\t34\t109\t398\t420\t22\t282\t294\nin the present study\t90\t15\t411\t514\t58\t95\t49\t50\t186\t73\t99\t66\t769\t19\nat the same time\t218\t168\t249\t474\t129\t202\t305\t53\t57\t71\t94\t12\t136\t44\nin terms of the\t172\t85\t216\t288\t95\t136\t173\t16\t20\t117\t187\t0\t85\t231\nas a result of\t105\t93\t201\t246\t80\t125\t220\t71\t93\t163\t132\t35\t312\t78\nin the absence of\t53\t17\t77\t79\t13\t23\t37\t49\t360\t100\t158\t41\t747\t108\nthe results of the\t70\t30\t379\t413\t138\t164\t111\t52\t45\t81\t129\t0\t216\t62\nwith respect to the\t57\t0\t159\t171\t23\t46\t45\t11\t81\t102\t308\t11\t161\t204\nin the context of\t148\t92\t187\t356\t81\t119\t293\t18\t35\t44\t84\t0\t129\t59\nthe fact that the\t120\t67\t187\t251\t45\t110\t106\t25\t68\t96\t138\t13\t148\t105\nthe nature of the\t127\t60\t311\t281\t68\t117\t108\t17\t63\t72\t81\t10\t156\t61\nit is important to\t109\t69\t183\t284\t64\t156\t120\t27\t51\t88\t113\t14\t158\t72\nin the form of\t98\t109\t156\t255\t83\t123\t138\t45\t31\t92\t142\t18\t116\t63\nthe use of the\t94\t106\t204\t353\t109\t134\t127\t15\t13\t35\t109\t0\t85\t49\na wide range of\t85\t96\t90\t202\t57\t120\t162\t34\t56\t106\t126\t21\t153\t59\nthe extent to which\t162\t103\t221\t310\t82\t155\t227\t14\t32\t32\t23\t0\t89\t11\ncan be used to\t55\t51\t52\t81\t30\t78\t38\t26\t38\t108\t242\t0\t114\t107\nit is possible that\t67\t19\t126\t125\t19\t67\t36\t25\t166\t74\t44\t21\t445\t19\nin addition to the\t66\t36\t115\t176\t20\t84\t67\t18\t92\t75\t98\t0\t191\t71\nthe total number of\t47\t10\t192\t137\t28\t95\t63\t25\t73\t30\t102\t13\t139\t65\nin table # the\t34\t0\t137\t136\t29\t60\t39\t58\t23\t118\t148\t40\t89\t53\nat the time of\t44\t34\t119\t181\t24\t77\t103\t43\t101\t60\t32\t0\t352\t11\nit is possible to\t66\t53\t62\t96\t24\t68\t26\t24\t17\t58\t203\t0\t74\t99\nat the beginning of\t73\t79\t94\t299\t57\t107\t134\t21\t37\t41\t74\t0\t66\t17\nfrom ## to ##\t24\t15\t166\t148\t36\t49\t61\t67\t72\t97\t74\t33\t226\t32\nthe case of the\t77\t29\t140\t154\t23\t60\t68\t22\t26\t53\t84\t15\t42\t107\nit should be noted\t62\t24\t119\t102\t27\t55\t29\t14\t48\t75\t129\t11\t155\t79\non the one hand\t98\t73\t160\t288\t50\t86\t127\t13\t0\t14\t19\t0\t22\t18\nto be able to\t74\t112\t97\t233\t81\t123\t92\t10\t27\t19\t44\t0\t32\t27\none of the most\t68\t73\t82\t196\t56\t59\t95\t24\t23\t57\t62\t12\t99\t41\na large number of\t48\t40\t65\t92\t29\t55\t42\t26\t48\t52\t113\t14\t132\t73\nhas been shown to\t10\t0\t43\t49\t0\t22\t17\t22\t142\t30\t46\t53\t603\t37\nthe size of the\t26\t13\t52\t23\t0\t20\t20\t31\t80\t53\t129\t0\t148\t96\na function of the\t15\t0\t55\t44\t0\t0\t20\t21\t30\t119\t195\t0\t93\t124\nto the fact that\t59\t37\t105\t160\t48\t74\t54\t12\t21\t37\t87\t10\t78\t63\nthat there is a\t91\t58\t101\t117\t51\t62\t58\t15\t18\t42\t58\t13\t107\t27\nthe basis of the\t47\t29\t148\t112\t43\t72\t68\t22\t31\t76\t62\t0\t128\t38\nwas found to be\t16\t14\t36\t38\t26\t36\t24\t30\t97\t54\t130\t31\t250\t55\nis based on the\t31\t30\t60\t93\t31\t39\t48\t29\t27\t54\t136\t0\t97\t57\nin this case the\t48\t14\t58\t70\t15\t36\t25\t13\t16\t63\t149\t0\t59\t104\nfor each of the\t45\t21\t141\t149\t26\t60\t57\t40\t46\t45\t71\t0\t132\t33\nthe rest of the\t68\t73\t53\t147\t40\t64\t80\t29\t48\t42\t52\t0\t59\t34\nshould be noted that\t47\t19\t95\t76\t19\t42\t23\t11\t43\t65\t115\t11\t138\t73\nthe effect of the\t18\t12\t74\t79\t10\t20\t25\t30\t43\t71\t159\t16\t146\t95",
                mode="r", theme="cobalt"),

            br(),

            h3("Basic statistics"),
            verbatimTextOutput("textarea.out"),

            br(),

            h3("Correlation"),
            verbatimTextOutput("correl.out"),

            br(),

            strong("Scatter plot matrices"),

            br(),

            plotOutput("corPlot"),

            br(),

            h3("Results of correspondence analysis"),
            verbatimTextOutput("correspresult.out"),

            br(),
            br(),

            h3("Plot"),
            plotOutput("rowPlot", height = "500px"),

            br(),

            plotOutput("colPlot", height = "500px"),

            br(),

            h4("Biplot"),
            plotOutput("makeBiPlot", height = "700px"),

            br(),
            br(),

            strong('R session info'),
            verbatimTextOutput("info.out")

            ),


        tabPanel("About",

            strong('Note'),
            p('This web application is developed with',
            a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
            ''),

            br(),

            strong('List of Packages Used'), br(),
            code('library(shiny)'),br(),
            code('library(shinyAce)'),br(),
            code('library(psych)'),br(),
            code('library(MASS)'),br(),

            br(),

            strong('Code'),

            p('Source code for this application is mostly based on',
            a("this website maintained by Dr. Tabata.", href='http://www.lang.osaka-u.ac.jp/~tabata/JAECS2004/multi.html', target="_blank")),

            p('The code for this web application is available at',
            a('GitHub.', href='https://github.com/mizumot/corresp', target="_blank")),

            p('If you want to run this code on your computer (in a local R session), run the code below:',
            br(),
            code('library(shiny)'),br(),
            code('runGitHub("corresp","mizumot")')
            ),

            br(),

            strong('Recommended'),
            br(),

            a("The handbook of Research in Foreign Language Learning and Teaching(Takeuchi & Mizumoto, 2012)", href='http://mizumot.com/handbook/', target="_blank"),

            br(),
            br(),

            p('To learn more about R, I suggest this excellent and free e-book (pdf),',
            a("A Guide to Doing Statistics in Second Language Research Using R,", href="http://cw.routledge.com/textbooks/9780805861853/guide-to-R.asp", target="_blank"),
            'written by Dr. Jenifer Larson-Hall.'),

            p('Also, if you are a cool Mac user and want to use R with GUI,',
            a("MacR", href="http://www.urano-ken.com/blog/2013/02/25/installing-and-using-macr/", target="_blank"),
            'is defenitely the way to go!'),

            br(),

            strong('Author'),
            p(a("Atsushi MIZUMOTO,", href="http://mizumot.com", target="_blank"),' Ph.D.',br(),
            'Associate Professor of Applied Linguistics',br(),
            'Faculty of Foreign Language Studies /',br(),
            'Graduate School of Foreign Language Education and Research,',br(),
            'Kansai University, Osaka, Japan'),

            br(),

            a(img(src="http://i.creativecommons.org/p/mark/1.0/80x15.png"), target="_blank", href="http://creativecommons.org/publicdomain/mark/1.0/"),

            p(br())

            )

))
))