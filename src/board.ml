open Chance 

let chance_commands = [
init_chance "Chance: Advancement" 200 "Advance to Go! Collect $200!" "Go";
init_chance "Chance: Advancement" 0 "Advance to Illinois Avenue! 
Collect $200 if you pass Go!" "Illinois Avenue";
init_chance "Chance: Advancement" 0 "Advance to St.Charles Place! 
Collect $200 if you pass Go!" "St. Charles Place";
init_chance "Chance: Advancement" 0 "Take a trip to Reading Railroad!
Collect $200 if you pass Go" "Reading Railroad";
init_chance "Chance: Advancement" 0 "Go directly to Jail!
Do not $200 if you pass Go" "Just Visiting";
init_chance "Chance: Money Made" 150 "Your building loan matures. Collect $150" "Current";
init_chance "Chance: Payment Required" 15 "Speeding Fine! Pay $15" "Current";
init_chance "Chance: Payment Required" 150 "Pay School Tax of $150" "Current";
init_chance "Chance: Move Backwards" 0 "Move backwards 3 spaces" "3";
init_chance "Get out of Jail Free" 0 "Acquire a Get of Jail Free Card" "Current";

]

let draw_chance_card (chance_cards) = 
  let idx = Random.int (List.length chance_cards) + 1 in 
  List.nth chance_cards idx

let board : Tile.tile list =
  [
    Go;
    Property (Property.init_property "Mediterranean Avenue" Brown 60);
    CommunityChest;
    Property (Property.init_property "Baltic Avenue" Brown 60);
    IncomeTax;
    Property (Property.init_property "Reading Railroad" Railroad 200);
    Property (Property.init_property "Oriental Avenue" LightBlue 100);
    Chance (draw_chance_card chance_commands);
    Property (Property.init_property "Vermont Avenue" LightBlue 100);
    Property (Property.init_property "Connecticut Avenue" LightBlue 120);
    JustVisiting;
    Property (Property.init_property "St. Charles Place" Magenta 140);
    Property (Property.init_property "Electric Company" Utility 150);
    Property (Property.init_property "States Avenue" Magenta 140);
    Property (Property.init_property "Virginia Avenue" Magenta 160);
    Property (Property.init_property "Pennsylvania Railroad" Railroad 200);
    Property (Property.init_property "St. James Place" Orange 180);
    CommunityChest;
    Property (Property.init_property "Tennessee Avenue" Orange 180);
    Property (Property.init_property "New York Avenue" Orange 200);
    FreeParking;
    Property (Property.init_property "Kentucky Avenue" Red 220);
    Chance (draw_chance_card chance_commands);
    Property (Property.init_property "Indiana Avenue" Red 220);
    Property (Property.init_property "Illinois Avenue" Red 240);
    Property (Property.init_property "B. & O. Railroad" Railroad 200);
    Property (Property.init_property "Atlantic Avenue" Yellow 260);
    Property (Property.init_property "Ventnor Avenue" Yellow 260);
    Property (Property.init_property "Water Works" Utility 150);
    Property (Property.init_property "Marvin Gardens" Yellow 280);
    GoToJail;
    Property (Property.init_property "Pacific Avenue" Green 300);
    Property (Property.init_property "North Carolina Avenue" Green 300);
    CommunityChest;
    Property (Property.init_property "Pennsylvania Avenue" Green 320);
    Property (Property.init_property "Short Line" Railroad 200);
    Chance (draw_chance_card chance_commands);
    Property (Property.init_property "Park Place" Blue 350);
    LuxuryTax;
    Property (Property.init_property "Boardwalk" Blue 400);
  ]

let purchased int list = []

let chance_names: string list = ["Chance: Advancement";
 "Chance: Payment Required"; "Chance: Money Made"; 
 "Chance: Get out of Jail Free"; ]

(*TODO: Add chance commands for the chance names above. Then randomly pick a chance name and 
   command correspondingly for the above board list*)

