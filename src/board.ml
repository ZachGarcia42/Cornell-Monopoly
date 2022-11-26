open Chance
open Chest

let chance_commands =
  [
    init_chance "Chance: Advancement" 200 "Advance to Go! Collect $200!" "Go" 0;
    init_chance "Chance: Advancement" 0
      "Advance to Illinois Avenue! \nCollect $200 if you pass Go!"
      "Illinois Avenue" 0;
    init_chance "Chance: Advancement" 0
      "Advance to St.Charles Place! \nCollect $200 if you pass Go!"
      "St. Charles Place" 0;
    init_chance "Chance: Advancement" 0
      "Take a trip to Reading Railroad!\nCollect $200 if you pass Go"
      "Reading Railroad" 0;
    init_chance "Chance: Advancement" 0
      "Go directly to Jail!\nDo not $200 if you pass Go" "Just Visiting" 0;
    init_chance "Chance: Money Made" 150
      "Your building loan matures. Collect $150" "Current" 0;
    init_chance "Chance: Payment Required" 15 "Speeding Fine! Pay $15" "Current"
     
      0;
    init_chance "Chance: Payment Required" 150 "Pay School Tax of $150"
      "Current" 0;
    init_chance "Chance: Move Backwards" 0 "Move backwards 3 spaces" "3" (-3);
    init_chance "Chance: Get out of Jail Free" 0
      "Acquire a Get of Jail Free Card" "Current" 0;
  ]

let community_chest_commands =
 
  [
    init_chest "Advance to Go! Collect $200" "Go" 200 "Bank";
    init_chest "Bank error in your favor! Collect $200" "Current" 200 "Bank";
    init_chest "Doctor's Fees! Pay $50" "Current" (-50) "Bank";
    init_chest "Get out of Jail Free Card earned" "Current" 0 "Bank";
    init_chest "It is your birthday! Collect $10 from every player" "Current" 10 "All Players"
     
      
    ]

let draw_card cards =
  let idx = Random.int (List.length cards) in 

  let draw_card cards =
    let idx = Random.int (List.length cards) in
    List.nth cards idx 
  
  in draw_card cards

let board : Tile.tile list =
  [
    Go;
    Property (Property.init_property "Low Rise 7" Brown 60 1);
    CommunityChest (draw_card community_chest_commands);
    Property (Property.init_property "Mary Donlon Hall" Brown 60 3);
    IncomeTax;
    Property (Property.init_property "Louie's Lunch" Railroad 200 5);
    Property (Property.init_property "West Campus Gothics" LightBlue 100 6);
    Chance (draw_card chance_commands);
    Property (Property.init_property "Clara Dickson Hall" LightBlue 100 8);
    Property (Property.init_property "Milstein Balls" LightBlue 120 9);
    JustVisiting;
    Property (Property.init_property "Baker Lab" Magenta 140 11);
    Property (Property.init_property "West Campus Plumbing" Utility 150 12);
    Property (Property.init_property "Goldwin Smith" Magenta 140 13);
    Property (Property.init_property "Ag Quad" Magenta 160 14);
    Property (Property.init_property "Mattin's Cafe" Railroad 200 15);
    Property (Property.init_property "Olin Hall" Orange 180 16);
    CommunityChest (draw_card community_chest_commands);
    Property (Property.init_property "Uris Hall" Orange 180 18);
    Property (Property.init_property "Uris Library" Orange 200 19);
    FreeParking;
    Property (Property.init_property "PSB" Red 220 21);
    Chance (draw_card chance_commands);
    Property (Property.init_property "Toni Morrison Hall" Red 220 22);
    Property (Property.init_property "Ganedago Hall" Red 240 24);
    Property (Property.init_property "Mac's Cafe" Railroad 200 25);
    Property (Property.init_property "Klarman Hall" Yellow 260 26);
    Property (Property.init_property "CTB" Yellow 260 27);
    Property (Property.init_property "North Campus Fire Alarms" Utility 150 28);
    Property (Property.init_property "Statler Hotel" Yellow 280 29);
    GoToJail;
    Property (Property.init_property "Mann Library" Green 300 31);
    Property (Property.init_property "Rhodes Hall" Green 300 32);
    CommunityChest (draw_card community_chest_commands);
    Property (Property.init_property "Duffield Hall" Green 320 34);
    Property
      (Property.init_property "Cornell's Botanical Gardens" Railroad 200 35);
    Chance (draw_card chance_commands);
    Property (Property.init_property "Upson Hall" Blue 350 37);
    LuxuryTax;
    Property (Property.init_property "Gates Hall" Blue 400 39);
  ]

let chance_names : string list =
  [
    "Chance: Advancement";
    "Chance: Payment Required";
    "Chance: Money Made";
    "Chance: Get out of Jail Free";
  ]

(*TODO: Add chance commands for the chance names above. Then randomly pick a
  chance name and command correspondingly for the above board list*)