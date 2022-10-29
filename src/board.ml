let board : Tile.tile list =
  [
    Go;
    Property (Property.init_property "Mediterranean Avenue" Brown 60);
    CommunityChest;
    Property (Property.init_property "Baltic Avenue" Brown 60);
    IncomeTax;
    Property (Property.init_property "Reading Railroad" Railroad 200);
    Property (Property.init_property "Oriental Avenue" LightBlue 100);
    Chance (Chance.init_chance "Chance: Advancement" 0 
    "Advance to Boardwalk! Collect $200 if passing Go.");
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
    Chance (Chance.init_chance "Chance: Advancement" 0 
    "Advance to Boardwalk! Collect $200 if passing Go.");
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
    Chance (Chance.init_chance "Chance: Advancement" 0 
    "Advance to Boardwalk! Collect $200 if passing Go.");
    Property (Property.init_property "Park Place" Blue 350);
    LuxuryTax;
    Property (Property.init_property "Boardwalk" Blue 400);
  ]

let purchased int list = []
