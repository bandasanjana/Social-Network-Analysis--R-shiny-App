# Social-Network-Analysis--R-shiny-App


Shiny app created to study social network of emails shared withing European union

Description of Dataset:

email-Eu-core network The network was generated using email data from a large European research institution. We have anonymized information about all incoming and outgoing email between members of the research institution. There is an edge (u, v) in the network if person u sent person v at least one email. The e-mails only represent communication between institution members (the core), and the dataset does not contain incoming messages from or outgoing messages to the rest of the world.

The dataset also contains "ground-truth" community memberships of the nodes. Each individual belongs to exactly one of 42 departments at the research institute.

This network represents the "core" of the email-EuAll network, which also contains links between members of the institution and people outside of the institution (although the node IDs are not the same).

File formats:
Links file: This should contain two columns. First column representing the origination of the link and second column being the target of the link.

Nodes File: Two columns, first one should represnt the node and second one should represent the group of the node



Use cases
This app can be used for various kinds of network analysis if files are uploaded in proper format:
Social Network: Who interatcs whine whom
Sports Network: For example, links can be the passes made between players(nodes) in a basketball match
People Analytics
Customer Network

