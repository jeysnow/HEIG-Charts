#Rector appointment process----
ggplot(HEIG_DATA, aes(x = Maintainer_Type, fill= factor(Rector_Appointment)))+
  geom_bar(position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")

ggplot(HEIG_DATA, aes(x=factor(HEI_Type), fill= factor(Rector_Appointment)))+
  geom_bar(position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")

ggplot(HEIG_DATA, aes(x=factor(Maintainer_Profit), fill= factor(Rector_Appointment)))+
  geom_bar(position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")

#Number of Boards----

ggplot(HEIG_DATA, aes(x= Maintainer_Type, fill= factor(Board_Amount)))+
  geom_bar(position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")

ggplot(HEIG_DATA, aes(x= HEI_Type, fill= factor(Board_Amount)))+
  geom_bar(position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")

ggplot(HEIG_DATA, aes(x= Maintainer_Profit, fill= factor(Board_Amount)))+
  geom_bar(position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")


#Stakeholder representation----

# chart of average stakeholder representation (color) by maintainer type
ggplot(REP_TABLE[Institutional_Category %in% levels(HEIG_DATA$Maintainer_Type) ]
       , aes( x = Institutional_Category, fill= Stakeholder))+
  geom_col(aes(y = Repres_Freq), position = "dodge" )+
  scale_y_continuous(labels=scales::percent)

# chart of average stakeholder representation (color) by HEI type
ggplot(REP_TABLE[Institutional_Category %in% levels(HEIG_DATA$HEI_Type) ]
       , aes( x = Institutional_Category, fill= Stakeholder))+
  geom_col(aes(y = Repres_Freq), position = "dodge" )+
  scale_y_continuous(labels=scales::percent)

# chart of average stakeholder representation (color) by remaining categories
ggplot(REP_TABLE[Institutional_Category %in% c("For-Profit","Nonprofit","Certif_Communitarian","Certif_Relig") ], 
       aes( x = Institutional_Category, fill= Stakeholder))+
  geom_col(aes(y = Repres_Freq), position = "dodge" )+
  scale_y_continuous(labels=scales::percent)

#Academic Governance----

# chart of stacked responsible parties by maintainer type, 
# aggregated by governance role using facet_Grid
ggplot(RESPONSIBILITY_TABLE[Institutional_Category %in% levels(HEIG_DATA$Maintainer_Type)][
  Governance_Theme == "Academic"],
       aes( x = Governance_Role, 
            y = Bylaw_Freq,
            fill= Responsible_party))+
  geom_col(position = "fill" )+
  facet_grid(.~Institutional_Category)+
  scale_y_continuous(labels=scales::percent)
  
# chart of stacked responsible parties by HEI type, 
# aggregated by governance role using facet_Grid
ggplot(RESPONSIBILITY_TABLE[Institutional_Category %in% levels(HEIG_DATA$HEI_Type)][
  Governance_Theme == "Academic"],
  aes( x = Governance_Role, 
       y = Bylaw_Freq,
       fill= Responsible_party))+
  geom_col(position = "fill" )+
  facet_grid(.~Institutional_Category)+
  scale_y_continuous(labels=scales::percent)

# chart of stacked responsible parties by remaining institutional factors, 
# aggregated by governance role using facet_Grid
ggplot(RESPONSIBILITY_TABLE[Institutional_Category %in% 
                              c("For-Profit","Nonprofit","Certif_Communitarian","Certif_Relig")][
  Governance_Theme == "Academic"],
  aes( x = Governance_Role, 
       y = Bylaw_Freq,
       fill= Responsible_party))+
  geom_col(position = "fill" )+
  facet_grid(.~Institutional_Category)+
  scale_y_continuous(labels=scales::percent)

#Financial Governance----

# chart of stacked responsible parties by maintainer type, 
# aggregated by governance role using facet_Grid
ggplot(RESPONSIBILITY_TABLE[Institutional_Category %in% levels(HEIG_DATA$Maintainer_Type)][
  Governance_Theme == "Financial"],
  aes( x = Governance_Role, 
       y = Bylaw_Freq,
       fill= Responsible_party))+
  geom_col(position = "fill" )+
  facet_grid(.~Institutional_Category)+
  scale_y_continuous(labels=scales::percent)

# chart of stacked responsible parties by HEI type, 
# aggregated by governance role using facet_Grid
ggplot(RESPONSIBILITY_TABLE[Institutional_Category %in% levels(HEIG_DATA$HEI_Type)][
  Governance_Theme == "Financial"],
  aes( x = Governance_Role, 
       y = Bylaw_Freq,
       fill= Responsible_party))+
  geom_col(position = "fill" )+
  facet_grid(.~Institutional_Category)+
  scale_y_continuous(labels=scales::percent)

# chart of stacked responsible parties by remaining institutional factors, 
# aggregated by governance role using facet_Grid
ggplot(RESPONSIBILITY_TABLE[Institutional_Category %in% 
                              c("For-Profit","Nonprofit","Certif_Communitarian","Certif_Relig")][
                                Governance_Theme == "Financial"],
       aes( x = Governance_Role, 
            y = Bylaw_Freq,
            fill= Responsible_party))+
  geom_col(position = "fill" )+
  facet_grid(.~Institutional_Category)+
  scale_y_continuous(labels=scales::percent)

#Organizational Governance----

# chart of stacked responsible parties by maintainer type, 
# aggregated by governance role using facet_Grid
ggplot(RESPONSIBILITY_TABLE[Institutional_Category %in% levels(HEIG_DATA$Maintainer_Type)][
  Governance_Theme == "Organizational"],
  aes( x = Governance_Role, 
       y = Bylaw_Freq,
       fill= Responsible_party))+
  geom_col(position = "fill" )+
  facet_grid(.~Institutional_Category)+
  scale_y_continuous(labels=scales::percent)

# chart of stacked responsible parties by HEI type, 
# aggregated by governance role using facet_Grid
ggplot(RESPONSIBILITY_TABLE[Institutional_Category %in% levels(HEIG_DATA$HEI_Type)][
  Governance_Theme == "Organizational"],
  aes( x = Governance_Role, 
       y = Bylaw_Freq,
       fill= Responsible_party))+
  geom_col(position = "fill" )+
  facet_grid(.~Institutional_Category)+
  scale_y_continuous(labels=scales::percent)

# chart of stacked responsible parties by remaining institutional factors, 
# aggregated by governance role using facet_Grid
ggplot(RESPONSIBILITY_TABLE[Institutional_Category %in% 
                              c("For-Profit","Nonprofit","Certif_Communitarian","Certif_Relig")][
                                Governance_Theme == "Organizational"],
       aes( x = Governance_Role, 
            y = Bylaw_Freq,
            fill= Responsible_party))+
  geom_col(position = "fill" )+
  facet_grid(.~Institutional_Category)+
  scale_y_continuous(labels=scales::percent)

#Staff Governance----

# chart of stacked responsible parties by maintainer type, 
# aggregated by governance role using facet_Grid
ggplot(RESPONSIBILITY_TABLE[Institutional_Category %in% levels(HEIG_DATA$Maintainer_Type)][
  Governance_Theme == "Staff"],
  aes( x = Governance_Role, 
       y = Bylaw_Freq,
       fill= Responsible_party))+
  geom_col(position = "fill" )+
  facet_grid(.~Institutional_Category)+
  scale_y_continuous(labels=scales::percent)

# chart of stacked responsible parties by HEI type, 
# aggregated by governance role using facet_Grid
ggplot(RESPONSIBILITY_TABLE[Institutional_Category %in% levels(HEIG_DATA$HEI_Type)][
  Governance_Theme == "Staff"],
  aes( x = Governance_Role, 
       y = Bylaw_Freq,
       fill= Responsible_party))+
  geom_col(position = "fill" )+
  facet_grid(.~Institutional_Category)+
  scale_y_continuous(labels=scales::percent)

# chart of stacked responsible parties by remaining institutional factors, 
# aggregated by governance role using facet_Grid
ggplot(RESPONSIBILITY_TABLE[Institutional_Category %in% 
                              c("For-Profit","Nonprofit","Certif_Communitarian","Certif_Relig")][
                                Governance_Theme == "Staff"],
       aes( x = Governance_Role, 
            y = Bylaw_Freq,
            fill= Responsible_party))+
  geom_col(position = "fill" )+
  facet_grid(.~Institutional_Category)+
  scale_y_continuous(labels=scales::percent)
