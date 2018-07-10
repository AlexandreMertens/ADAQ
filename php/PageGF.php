<script src="//code.jquery.com/jquery-1.11.1.min.js"></script>

<!DOCTYPE html>
<html lang="fr" dir="ltr">
  <head>
    <meta charset="utf-8">
    <title>Greenfeed</title>
    <link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css" rel="stylesheet" id="bootstrap-css">
    <script src="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"></script>
    <link rel="stylesheet" href="css/bootstrap.min.css">
		<link rel="stylesheet" href="css/style.css">
    <link rel="shortcut icon" type="image/x-icon" href="favicon.png" />
  </head>
  <body>
    <div class="container-fluide">

    <!-------->
    <div id="content">
        <ul id="tabs" class="nav nav-tabs onglet" data-tabs="tabs">
            <li><a href="#red" data-toggle="tab">Greenfeed 90</a></li>
            <li class="active"><a href="#orange" data-toggle="tab">Greenfeed 91</a></li>
        </ul>
        <div id="my-tab-content" class="tab-content">
          <!----------Greenfeed 90--------->
            <div class="tab-pane" id="red">
              <div class="container-fluid entête">
                <div class="row">
                  <div class="col-md-1 bouton">
                    <?php
                    $nomcol='colData_90';
                    $len_col=count(file($nomcol));
                    if (substr(cellule($nomcol,$len_col,1),0,6)=='Online')
                      echo'<img src="image/bouton_online.png" alt="logo online" width=75px />';
                    elseif (cellule($nomcol,$len_col,1)=='Offline')
                      echo'<img src="image/bouton_offline.png" alt="logo online" width=75px />';
                    else
                      echo'<img src="image/bouton_autre.png" alt="logo online" width=75px />';
                    ?>
                  </div>
                  <div class="col-md-4 statut">
                    <?php
                    $len_col=count(file($nomcol));
                    echo cellule($nomcol,$len_col,0).' :  '.cellule($nomcol,$len_col,1);
                    ?>
                  </div>
                  <div class="col-md-6 colonne-centree">
                    <img src="image/Logo_CRA-W.png" alt="logo CRA-W" />
                  </div>
                  <div class="col-md-1 statut">
                    <?php
                    $len_col=count(file($nomcol));

                    $datetime1 = new DateTime(cellule($nomcol,$len_col-1,1));
                    $datetime2 = new DateTime(date("Y-m-d"));
                    $interval = $datetime1->diff($datetime2);
                    $maj=(cellule($nomcol,$len_col-1,1));
                    $c =$interval->format('%a');
                    if($c < 1){
                      $couleur = "#ccc";
                    }
                    else {
                      $couleur = "red";
                    }
                    echo"<font color=".$couleur.">$maj</font>";
                    ?>
                  </div>
                </div>
              </div>
              <div class="container">
                <div class="row contenu">
                  <div class="col-md-4 col">
                    <div class="row">
                      <div class="col-md-6 nomcol">
                        <?php
                        $len_col=count(file($nomcol));
                        for ($k=1;$k<=$len_col-2;$k++){
                          echo cellule($nomcol,$k,0)." :   <br/><br/>";
                        }
                         ?>
                      </div>
                      <div class="col-md-6 valcol">
                        <?php
                        $len_col=count(file($nomcol));
                        for ($k=1;$k<=$len_col-2;$k++){
                          $valeur=cellule($nomcol,$k,1)."<br/><br/>";
                          $couleurv=cellule($nomcol,$k,2);
                          echo"<font color=".$couleurv.">$valeur</font>";
                        }
                         ?>
                      </div>

                    </div>

                  </div>
                  <div class="col-md-4 tableau">

                    <table class="table table-striped">
                      <thead>
                        <tr>
                          <?php
                          $nomtab='Tableau_90';
                          for ($i=0;$i<=10;$i++){
                            echo"<td>".cellule($nomtab,1,$i)."</td>";
                          } ?>
                        </tr>
                      </thead>
                      <tbody>
                        <?php
                        $len_colt=count(file($nomtab));
                        for ($k=2;$k<=$len_colt;$k++){
                          echo "<tr>";
                          for($i=0;$i<=10;$i++){
                            echo '<td>'.cellule($nomtab,$k,$i).'</td>';
                          } echo '</tr>';
                        }
                        ?>
                      </tbody>
                    </table>

                  </div>
                </div>

              </div>
            </div>
          <!----------Greenfeed 91--------->
            <div class="tab-pane active" id="orange">
              <div class="container-fluid entête">
                <div class="row">
                  <div class="col-md-1 bouton">
                    <?php
                    $nomcol='colData_91';
                    $len_col=count(file($nomcol));
                    if (substr(cellule($nomcol,$len_col,1),0,6)=='Online')
                      echo'<img src="image/bouton_online.png" alt="logo online" width=75px />';
                    elseif (cellule($nomcol,$len_col,1)=='Offline')
                      echo'<img src="image/bouton_offline.png" alt="logo online" width=75px />';
                    else
                      echo'<img src="image/bouton_autre.png" alt="logo online" width=75px />';
                    ?>
                  </div>
                  <div class="col-md-4 statut">
                    <?php
                    $len_col=count(file($nomcol));
                    echo cellule($nomcol,$len_col,0).' :  '.cellule($nomcol,$len_col,1);
                    ?>
                  </div>
                  <div class="col-md-6 colonne-centree">
                    <img src="image/Logo_CRA-W.png" alt="logo CRA-W" />
                  </div>
                  <div class="col-md-1 statut">
                    <?php
                    $len_col=count(file($nomcol));

                    $datetime1 = new DateTime(cellule($nomcol,$len_col-1,1));
                    $datetime2 = new DateTime(date("Y-m-d"));
                    $interval = $datetime1->diff($datetime2);
                    $maj=(cellule($nomcol,$len_col-1,1));
                    $c =$interval->format('%a');
                    if($c < 1){
                      $couleur = "#ccc";
                    }
                    else {
                      $couleur = "red";
                    }
                    echo"<font color=".$couleur.">$maj</font>";
                    ?>
                  </div>
                </div>
              </div>
              <div class="container">
                <div class="row contenu">
                  <div class="col-md-4 col">
                    <div class="row">
                      <div class="col-md-6 nomcol">
                        <?php
                        $len_col=count(file($nomcol));
                        for ($k=1;$k<=$len_col-2;$k++){
                          echo cellule($nomcol,$k,0)." :   <br/><br/>";
                        }
                         ?>
                      </div>
                      <div class="col-md-6 valcol">
                        <?php
                        $len_col=count(file($nomcol));
                        for ($k=1;$k<=$len_col-2;$k++){
                          $valeur=cellule($nomcol,$k,1)."<br/><br/>";
                          $couleurv=cellule($nomcol,$k,2);
                          echo"<font color=".$couleurv.">$valeur</font>";
                        }
                         ?>
                      </div>

                    </div>

                  </div>
                  <div class="col-md-4 tableau">

                    <table class="table table-striped">
                      <thead>
                        <tr>
                          <?php
                          $nomtab='Tableau_91';
                          for ($i=0;$i<=10;$i++){
                            echo"<td>".cellule($nomtab,1,$i)."</td>";
                          } ?>
                        </tr>
                      </thead>
                      <tbody>
                        <?php
                        $len_colt=count(file($nomtab));
                        for ($k=2;$k<=$len_colt;$k++){
                          echo "<tr>";
                          for($i=0;$i<=10;$i++){
                            echo '<td>'.cellule($nomtab,$k,$i).'</td>';
                          } echo '</tr>';
                        }
                        ?>
                      </tbody>
                    </table>

                  </div>
                </div>

              </div>
              <?php
              //lecture d'un fichier cellule par cellule
              //ligne 1-10
              //colonne 0-9
              function cellule($csv, $lig, $col){
              	$fic=fopen($csv,'r');
              	$len=count(file($csv));
              	for ($i=1;$i<=$lig;$i++){
              		$ligne=fgetcsv($fic,1024,',');
              	}
              	fclose($fic) ;
              	return $ligne[$col];
              }
              ?>
            </div>
        </div>
    </div>
  </body>
</html>
