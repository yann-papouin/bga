unit CONLib;

interface

uses
  Windows;

type

  TBfData = string[255];

  TBf2Data  = record
    X : single;
    Y : single;
  end;

  TBf3Data = record
    X : single;
    Y : single;
    Z : single;
  end;

  TBf4Data = record
    X : single;
    Y : single;
    Z : single;
    W : single;
  end;



  TBfAbsolutePosition = TBf3Data;

  TBfObjectTemplate = class(TObject)
(*
  ObjectTemplate.Acceleration  [Vietnam]
   ObjectTemplate.Accumulate [Vietnam]
   ObjectTemplate.Active
   ObjectTemplate.ActiveEffectPersistancePerFrame
   ObjectTemplate.ActiveHeatIncrement
   ObjectTemplate.AddAmmoType
   ObjectTemplate.AddArmorEffect
   ObjectTemplate.AddChild
   ObjectTemplate.AddDevFire
   ObjectTemplate.AddDevJump
   ObjectTemplate.AddDevPitch
   ObjectTemplate.AddDevRun
   ObjectTemplate.AddDevStrafe
   ObjectTemplate.AddDevWalk
   ObjectTemplate.AddDevYaw
   ObjectTemplate.AddEmitterSpeed
   ObjectTemplate.AddFireArmsPosition
   ObjectTemplate.AddLinePoint
   ObjectTemplate.AddObjectiveSpawnerToComposite
   ObjectTemplate.AddPcoPosId
   ObjectTemplate.AddRootSpeed
   ObjectTemplate.AddScaleEffectTemplate [Vietnam]
   ObjectTemplate.AddSkeletonIK
   ObjectTemplate.AddTemplate
   ObjectTemplate.AddToCollisionGroup
   ObjectTemplate.AddToColMatList [Vietnam]
   ObjectTemplate.AddToNegativeMask
   ObjectTemplate.AddToProjectileList
   ObjectTemplate.AddVehicleType
   ObjectTemplate.AddWeaponIcon
   ObjectTemplate.AddWorkOnMaterial
   ObjectTemplate.AirFlowAffect [Vietnam]
   ObjectTemplate.AiTemplate
   ObjectTemplate.AllowReloadOnEmptyClipOnly
   ObjectTemplate.AllowRestart [Vietnam]
   ObjectTemplate.AlphaOverTime
   ObjectTemplate.AlphaTestRef
   ObjectTemplate.AlphaTestRefOverTime [Vietnam]
   ObjectTemplate.AltFireOnce
   ObjectTemplate.Altweapon [Vietnam]
   ObjectTemplate.AltWeaponindex [Vietnam]
   ObjectTemplate.AmmoBar [Vietnam]
   ObjectTemplate.AmmoBarFill [Vietnam]
   ObjectTemplate.AmmoBarPosX [Vietnam]
   ObjectTemplate.AmmoBarPosY [Vietnam]
   ObjectTemplate.AmmoBarSize [Vietnam]
   ObjectTemplate.AmmoBarTextPosX [Vietnam]
   ObjectTemplate.AmmoBarTextPosY [Vietnam]
   ObjectTemplate.AmmoIcon [Vietnam]
   ObjectTemplate.AmmoType
   ObjectTemplate.AngleMod
   ObjectTemplate.AnimationSpeed
   ObjectTemplate.AnimationSpeedOverTime
   ObjectTemplate.AreaValue
   ObjectTemplate.ArtPos
   ObjectTemplate.AsynchronyFire [Vietnam]
   ObjectTemplate.AttachRadius [Vietnam]
   ObjectTemplate.AttachToListener [Vietnam]
   ObjectTemplate.AttackSpeed [Vietnam]
   ObjectTemplate.AutomaticPitchStabilization
   ObjectTemplate.AutomaticReset [Vietnam]
   ObjectTemplate.AutomaticYawStabilization
   ObjectTemplate.AutoReload
   ObjectTemplate.AutoShutOff [Vietnam]
   ObjectTemplate.AutoStart [Vietnam]
   ObjectTemplate.BindToSkeletonPart
   ObjectTemplate.BlastAmmoCount [Vietnam]
   ObjectTemplate.BoneName [Vietnam]
   ObjectTemplate.BurstFrequency
   ObjectTemplate.CameraDelayDistance
   ObjectTemplate.CameraDelayTime
   ObjectTemplate.CanBeRepaiedAsDestroyed
   ObjectTemplate.CanConvertToAmmo [Vietnam]
   ObjectTemplate.CantSelectWhenNoAmmo
   ObjectTemplate.CEBufferSize [Vietnam]
   ObjectTemplate.CEeffectRadius [Vietnam]
   ObjectTemplate.CEframedamage [Vietnam]
   ObjectTemplate.Center1pHands
   ObjectTemplate.ChangeWeaponWhenNoAmmo
   ObjectTemplate.ClusterRecDepth [Vietnam]
   ObjectTemplate.ColorRGBAOverTime
   ObjectTemplate.ConstantRpm [Vietnam]
   ObjectTemplate.ContinousRotationSpeed [Vietnam]
   ObjectTemplate.ControlPointIcon [Vietnam]
   ObjectTemplate.CoolDownPerSec
   ObjectTemplate.CoolingFactor
   ObjectTemplate.CopyLinksCount [Vietnam]
   ObjectTemplate.Create
   ObjectTemplate.CreateInvisible
   ObjectTemplate.CreateNotInGrid
   ObjectTemplate.CreateProjectilesOnDemand [Vietnam]
   ObjectTemplate.CreateSkeleton
   ObjectTemplate.CriticalDamage
   ObjectTemplate.CrossHairType [Vietnam]
   ObjectTemplate.CullRadiusScale
   ObjectTemplate.CVMChase
   ObjectTemplate.CVMExternTrace
   ObjectTemplate.CVMFlyBy
   ObjectTemplate.CVMFrontChase
   ObjectTemplate.CVMInside
   ObjectTemplate.CVMTrace
   ObjectTemplate.DamageFromWater
   ObjectTemplate.DamageType
   ObjectTemplate.DamageWhenLost
   ObjectTemplate.Damping
   ObjectTemplate.DampingConstant [Vietnam]
   ObjectTemplate.DeathEffectName
   ObjectTemplate.DecreaseAngleToZero [Vietnam]
   ObjectTemplate.DefaultAngleOfAttack [Vietnam]
   ObjectTemplate.DefaultEffectTemplate
   ObjectTemplate.DegenerationFactor [Vietnam]
   ObjectTemplate.Delay
   ObjectTemplate.DelayToUse
   ObjectTemplate.DestBlendMode
   ObjectTemplate.Destroyed
   ObjectTemplate.DestroyOnExit
   ObjectTemplate.DestroyVehicleWhenNoAmmo
   ObjectTemplate.DetectionRadius
   ObjectTemplate.DetectionTime [Vietnam]
   ObjectTemplate.DetonateAltWeapon [Vietnam]
   ObjectTemplate.DetonateOnWaterCollision
   ObjectTemplate.DieAfterColl
   ObjectTemplate.Differential [Vietnam]
   ObjectTemplate.DirBarRotate
   ObjectTemplate.DirBarXScale
   ObjectTemplate.DirBarYScaleAbove
   ObjectTemplate.DirBarYScaleBelow
   ObjectTemplate.DirBarYScaleMax
   ObjectTemplate.DirBarYScaleMin
   ObjectTemplate.DisableIfEnemyInsideRadius
   ObjectTemplate.DisableSpawnPointsOnEnter
   ObjectTemplate.DisableWhenFired
   ObjectTemplate.DisableWhenLosingControl
   ObjectTemplate.DisarmMinesDistance
   ObjectTemplate.Distance
   ObjectTemplate.DistanceLimit [Vietnam]
   ObjectTemplate.DistToMinDamage
   ObjectTemplate.DistToStartLoseDamage
   ObjectTemplate.DontClearTeamOnExit
   ObjectTemplate.DownAngleMultiplier
   ObjectTemplate.Drag
   ObjectTemplate.DragModifier [Vietnam]
   ObjectTemplate.DragOffset
   ObjectTemplate.DragOverTime
   ObjectTemplate.DrawAttachRadius [Vietnam]
   ObjectTemplate.DropSpeed [Vietnam]
   ObjectTemplate.EffectDissipationTime [Vietnam]
   ObjectTemplate.EffectFrequency [Vietnam]
   ObjectTemplate.EffectGrades [Vietnam]
   ObjectTemplate.EffectID [Vietnam]
   ObjectTemplate.EffectSeperation [Vietnam]
   ObjectTemplate.EjectClipTime
   ObjectTemplate.EmitterSpeedScale
   ObjectTemplate.EnableRadarMode
   ObjectTemplate.EndEffectTemplate
   ObjectTemplate.EngineType [Vietnam]
   ObjectTemplate.EntryRadius [Vietnam]
   ObjectTemplate.ExitSpeedMod
   ObjectTemplate.ExitTimer
   ObjectTemplate.ExplodeNearEnemyDistance
   ObjectTemplate.ExplosionDamage
   ObjectTemplate.ExplosionForce
   ObjectTemplate.ExplosionForceMax
   ObjectTemplate.ExplosionForceMod
   ObjectTemplate.ExplosionRadius
   ObjectTemplate.FadeAtTimeToLiveAfterDeath
   ObjectTemplate.FireDelay
   ObjectTemplate.FireInCameraDof
   ObjectTemplate.FireingForce
   ObjectTemplate.FireOnce
   ObjectTemplate.FirstCollisionEffect [Vietnam]
   ObjectTemplate.FlagTemplate
   ObjectTemplate.FlapLift [Vietnam]
   ObjectTemplate.FloatMaxLift [Vietnam]
   ObjectTemplate.FloatMinLift [Vietnam]
   ObjectTemplate.ForceEmission [Vietnam]
   ObjectTemplate.ForceOnExplosion
   ObjectTemplate.GearChangeTime [Vietnam]
   ObjectTemplate.GearDown [Vietnam]
   ObjectTemplate.GearDownEngineInput [Vietnam]
   ObjectTemplate.GearDownHeight [Vietnam]
   ObjectTemplate.GearUp [Vietnam]
   ObjectTemplate.GearUpEngineInput [Vietnam]
   ObjectTemplate.Geometry
   ObjectTemplate.GravityModifier
   ObjectTemplate.GravityModifierOverTime
   ObjectTemplate.Grip
   ObjectTemplate.GUIIndex
   ObjectTemplate.HardColDotLimit [Vietnam]
   ObjectTemplate.HardColSpeedMinMagnitude [Vietnam]
   ObjectTemplate.HasArmor
   ObjectTemplate.HasCollisionEffect
   ObjectTemplate.HasCollisionPhysics
   ObjectTemplate.HasDynamicShadow
   ObjectTemplate.HasGyroScope [Vietnam]
   ObjectTemplate.HasMag [Vietnam]
   ObjectTemplate.HasMobilePhysics
   ObjectTemplate.HasOnTimeEffect
   ObjectTemplate.HasOverDamage
   ObjectTemplate.Haspointphysics
   ObjectTemplate.HasResponsePhysics
   ObjectTemplate.HasRestrictedExit
   ObjectTemplate.HasStartEffect
   ObjectTemplate.HasStaticColor
   ObjectTemplate.HasTarget [Vietnam]
   ObjectTemplate.HasTurretIcon [Vietnam]
   ObjectTemplate.HeadIcon [Vietnam]
   ObjectTemplate.HeadMaxAngleMultiplier
   ObjectTemplate.HealDistance
   ObjectTemplate.HealFactor
   ObjectTemplate.HealthBarFullIcon [Vietnam]
   ObjectTemplate.HealthBarIcon [Vietnam]
   ObjectTemplate.HeatAddWhenFire
   ObjectTemplate.HeatIncrement
   ObjectTemplate.HeatMaxTurnForce [Vietnam]
   ObjectTemplate.HeatMaxTurnForceVsAir [Vietnam]
   ObjectTemplate.HideDuringFireTime
   ObjectTemplate.HitParalyzeTime [Vietnam]
   ObjectTemplate.Hitpoints
   ObjectTemplate.HoldObject
   ObjectTemplate.HookLength [Vietnam]
   ObjectTemplate.HookUpTime [Vietnam]
   ObjectTemplate.HookWidth [Vietnam]
   ObjectTemplate.HorizontalDampAngle [Vietnam]
   ObjectTemplate.HorizontalSpeedMagnifier [Vietnam]
   ObjectTemplate.HpLostWhileCriticalDamage
   ObjectTemplate.HpLostWhileDamageFromWater
   ObjectTemplate.HpLostWhileUpSideDown
   ObjectTemplate.HudAmmoType [Vietnam]
   ObjectTemplate.HullHeight [Vietnam]
   ObjectTemplate.InertiaModifier
   ObjectTemplate.InitAnimationFrame
   ObjectTemplate.InitRotation
   ObjectTemplate.InputFire [Vietnam]
   ObjectTemplate.InputToDropWire [Vietnam]
   ObjectTemplate.InputToPitch [Vietnam]
   ObjectTemplate.InputToRelease [Vietnam]
   ObjectTemplate.InputToRoll [Vietnam]
   ObjectTemplate.InputToYaw [Vietnam]
   ObjectTemplate.Intensity
   ObjectTemplate.IntensityAtSpeed
   ObjectTemplate.IntensityOverTime
   ObjectTemplate.InverseHeatBar
   ObjectTemplate.Invisible
   ObjectTemplate.InvisibleAtEndEffect
   ObjectTemplate.IsCE [Vietnam]
   ObjectTemplate.IsCluster [Vietnam]
   ObjectTemplate.IsHeatSeeking [Vietnam]
   ObjectTemplate.IsPortalPassing
   ObjectTemplate.IsSpawnEffect
   ObjectTemplate.IsSticky [Vietnam]
   ObjectTemplate.IsVehicleTrap [Vietnam]
   ObjectTemplate.ItemIndex
   ObjectTemplate.Iterate [Vietnam]
   ObjectTemplate.LeftAngleMultiplier
   ObjectTemplate.Length [Vietnam]
   ObjectTemplate.LoadSoundScript
   ObjectTemplate.LodDistance
   ObjectTemplate.LodSelector
   ObjectTemplate.Looping
   ObjectTemplate.LoseControlWhenEnemyClose
   ObjectTemplate.LoseControlWhenNotClose
   ObjectTemplate.MagSize
   ObjectTemplate.MagType
   ObjectTemplate.Mass
   ObjectTemplate.Material
   ObjectTemplate.Material2
   ObjectTemplate.MaxAngleOfAttack [Vietnam]
   ObjectTemplate.MaxClusterNum [Vietnam]
   ObjectTemplate.MaxDeviation
   ObjectTemplate.MaxDistanceUnderwaterSurface
   ObjectTemplate.MaxHeatTTL [Vietnam]
   ObjectTemplate.MaxHeight [Vietnam]
   ObjectTemplate.Maxhitpoints
   ObjectTemplate.MaxNrOfObjectSpawned
   ObjectTemplate.MaxRotation [Vietnam]
   ObjectTemplate.MaxSpawnDelay
   ObjectTemplate.MaxSpeed [Vietnam]
   ObjectTemplate.MaxTension [Vietnam]
   ObjectTemplate.MaxTimeToEffect [Vietnam]
   ObjectTemplate.MaxVertRegAngle [Vietnam]
   ObjectTemplate.MaxWireLength [Vietnam]
   ObjectTemplate.MinDamage
   ObjectTemplate.MinDevCrouching
   ObjectTemplate.MinDeviation
   ObjectTemplate.MinDevLying
   ObjectTemplate.MinDevStanding
   ObjectTemplate.MinDistanceUnderwaterSurface
   ObjectTemplate.MinGeneratedParticles [Vietnam]
   ObjectTemplate.MinimapIcon [Vietnam]
   ObjectTemplate.MinRotation [Vietnam]
   ObjectTemplate.MinSpawnDelay
   ObjectTemplate.MoveToWaterSurface
   ObjectTemplate.Name
   ObjectTemplate.NameTagOffset
   ObjectTemplate.NetworkableInfo
   ObjectTemplate.NoCollisionsAsDestroyed
   ObjectTemplate.NoFFSound
   ObjectTemplate.NoPhysics
   ObjectTemplate.NoPropellerEffectAtSpeed [Vietnam]
   ObjectTemplate.NoVertRegAngle [Vietnam]
   ObjectTemplate.NrOfObjectToSpawn
   ObjectTemplate.NumAnimationFrames
   ObjectTemplate.NumberOfGears [Vietnam]
   ObjectTemplate.NumberOfWeaponIcons [Vietnam]
   ObjectTemplate.NumOfMag
   ObjectTemplate.ObjectiveName
   ObjectTemplate.ObjectSpawnerId
   ObjectTemplate.ObjectTemplate
   ObjectTemplate.OnlyTakeableByTeam
   ObjectTemplate.OutsideHudOffset
   ObjectTemplate.OverrideAirMovementInhibitations
   ObjectTemplate.PassengerDownAngle [Vietnam]
   ObjectTemplate.PassengerTurnLeftAngle [Vietnam]
   ObjectTemplate.PassengerTurnRightAngle [Vietnam]
   ObjectTemplate.PassengerUpAngle [Vietnam]
   ObjectTemplate.PassiveHeatIncrement
   ObjectTemplate.PcoId
   ObjectTemplate.PitchOffset [Vietnam]
   ObjectTemplate.PivotPosition [Vietnam]
   ObjectTemplate.PortalPassingPosition
   ObjectTemplate.Position [Vietnam]
   ObjectTemplate.PositionalSpeedInDof
   ObjectTemplate.PositionalSpeedInRight
   ObjectTemplate.PositionalSpeedInUp
   ObjectTemplate.PositionOffset [Vietnam]
   ObjectTemplate.PrimaryAmmoBar [Vietnam]
   ObjectTemplate.PrimaryAmmoIcon [Vietnam]
   ObjectTemplate.Projectile2Template
   ObjectTemplate.ProjectilePosition
   ObjectTemplate.ProjectileTemplate
   ObjectTemplate.ProjectileType
   ObjectTemplate.ProjectileVisible
   ObjectTemplate.PropRotAxis [Vietnam]
   ObjectTemplate.ProximityFusePrimer
   ObjectTemplate.PureRotational [Vietnam]
   ObjectTemplate.RadioLanguage [Vietnam]
   ObjectTemplate.Radius
   ObjectTemplate.RaiseSpeed [Vietnam]
   ObjectTemplate.RecoilSize
   ObjectTemplate.RecoilSpeed
   ObjectTemplate.RegulatePitch [Vietnam]
   ObjectTemplate.RegulatePitchInput [Vietnam]
   ObjectTemplate.RegulateRoll [Vietnam]
   ObjectTemplate.RegulateRollInput [Vietnam]
   ObjectTemplate.RegulateVerticalPos [Vietnam]
   ObjectTemplate.RegulateVerticalPosInput [Vietnam]
   ObjectTemplate.RegulateYaw [Vietnam]
   ObjectTemplate.RegulateYawInput [Vietnam]
   ObjectTemplate.RelativePositionInDof
   ObjectTemplate.RelativePositionInRight
   ObjectTemplate.RelativePositionInUp
   ObjectTemplate.Reloadtime
   ObjectTemplate.RememberExcessInput
   ObjectTemplate.RemoteEngineInput [Vietnam]
   ObjectTemplate.RemoveAtRestart
   ObjectTemplate.RepairDistance
   ObjectTemplate.RepairFactor
   ObjectTemplate.ResetWhenRemoved
   ObjectTemplate.RightAngleMultiplier
   ObjectTemplate.Rotation [Vietnam]
   ObjectTemplate.RotationalSpeed
   ObjectTemplate.RotationalSpeedInDof
   ObjectTemplate.RotationalSpeedInRight
   ObjectTemplate.RotationalSpeedInUp
   ObjectTemplate.RotationSpeed
   ObjectTemplate.RotationSpeedOverTime
   ObjectTemplate.RoundOfFire
   ObjectTemplate.SaveInSeparateFile
   ObjectTemplate.ScanForEnemySonars
   ObjectTemplate.ScopeIcon [Vietnam]
   ObjectTemplate.SeatAnimationLowerBody
   ObjectTemplate.SeatAnimationUpperBody
   ObjectTemplate.SeatFlags
   ObjectTemplate.SecondaryAmmoBar [Vietnam]
   ObjectTemplate.SecondaryAmmoIcon [Vietnam]
   ObjectTemplate.SecondSpawnGroupId
   ObjectTemplate.SelfHealFactor
   ObjectTemplate.Set1pFov
   ObjectTemplate.SetAcceleration
   ObjectTemplate.SetActive
   ObjectTemplate.SetActiveAcceleration
   ObjectTemplate.SetAIEnterOnSpawn
   ObjectTemplate.SetAmmoBar
   ObjectTemplate.SetAmmoBarFill
   ObjectTemplate.SetAmmoBarPosX
   ObjectTemplate.SetAmmoBarPosY
   ObjectTemplate.SetAmmoBarSize
   ObjectTemplate.SetAmmoBarTextPosX
   ObjectTemplate.SetAmmoBarTextPosY
   ObjectTemplate.SetAmmoIcon
   ObjectTemplate.SetAmomBarPosX
   ObjectTemplate.SetAmomBarPosY
   ObjectTemplate.SetAmomBarTextPosX
   ObjectTemplate.SetAmomBarTextPosY
   ObjectTemplate.SetAnimatedTextureSpeed
   ObjectTemplate.SetAnimationState
   ObjectTemplate.SetAsynchronyFire
   ObjectTemplate.SetAttachToListener
   ObjectTemplate.SetAutomaticPitchStabilization
   ObjectTemplate.SetAutomaticReset
   ObjectTemplate.SetAutomaticYawStabilization
   ObjectTemplate.SetBackFlareCount
   ObjectTemplate.SetBlastAmmoCount
   ObjectTemplate.SetBoneName
   ObjectTemplate.SetBoneOriginOffset
   ObjectTemplate.SetCharacterHeight
   ObjectTemplate.SetContinousRotationSpeed
   ObjectTemplate.SetControlPointIcon
   ObjectTemplate.SetControlPointName
   ObjectTemplate.SetCopyLinksCount
   ObjectTemplate.SetCoronaColor
   ObjectTemplate.SetCoronaCount
   ObjectTemplate.SetCoronaDestBlend
   ObjectTemplate.Setcoronafadeall
   ObjectTemplate.SetCoronaRot
   ObjectTemplate.SetCoronaScale
   ObjectTemplate.SetCoronaSize
   ObjectTemplate.SetCoronaSrcBlend
   ObjectTemplate.SetCoronaTexture
   ObjectTemplate.SetCrossHairType
   ObjectTemplate.SetDamping
   ObjectTemplate.SetDevMod
   ObjectTemplate.SetDifferential
   ObjectTemplate.SetDragModifier
   ObjectTemplate.SetEngineType
   ObjectTemplate.SetEnterOnSpawn
   ObjectTemplate.SetEntryRadius
   ObjectTemplate.SetFireCameraShakeAnimationState
   ObjectTemplate.SetFireDev
   ObjectTemplate.SetFlagLocation
   ObjectTemplate.SetFlapLift
   ObjectTemplate.SetFlareColor
   ObjectTemplate.SetFlareDestBlend
   ObjectTemplate.SetFlareDistFadeScale
   ObjectTemplate.Setflarefadeall
   ObjectTemplate.SetFlareRot
   ObjectTemplate.SetFlareScale
   ObjectTemplate.SetFlareSize
   ObjectTemplate.SetFlareSrcBlend
   ObjectTemplate.SetFlareTexture
   ObjectTemplate.SetFloatMaxLift
   ObjectTemplate.SetFloatMinLift
   ObjectTemplate.SetForwardMod
   ObjectTemplate.SetGearChangeTime
   ObjectTemplate.SetGearDown
   ObjectTemplate.SetGearDownEngineInput
   ObjectTemplate.SetGearDownHeight
   ObjectTemplate.SetGearUp
   ObjectTemplate.SetGearUpEngineInput
   ObjectTemplate.SetGearUpHeight
   ObjectTemplate.SetGoBackOnRecoil
   ObjectTemplate.SetGroup
   ObjectTemplate.SetHasCollisionPhysics
   ObjectTemplate.SetHasMag
   ObjectTemplate.SetHasMobilePhysics
   ObjectTemplate.SetHasPointPhysics
   ObjectTemplate.SetHasRecoilForce
   ObjectTemplate.SetHasResponsePhysics
   ObjectTemplate.SetHasTarget
   ObjectTemplate.SetHasTurretIcon
   ObjectTemplate.SetHealth
   ObjectTemplate.SetHealthBarFullIcon
   ObjectTemplate.SetHealthBarIcon
   ObjectTemplate.SetHeatBarType
   ObjectTemplate.SetHudAmmoType
   ObjectTemplate.SetHullHeight
   ObjectTemplate.SetInAirAnimOverride
   ObjectTemplate.SetInAirAnims
   ObjectTemplate.SetInputFire
   ObjectTemplate.SetInputId
   ObjectTemplate.SetInputToPitch
   ObjectTemplate.SetInputToRoll
   ObjectTemplate.SetInputToYaw
   ObjectTemplate.SetIsFirstPersonPart
   ObjectTemplate.SetKitAccelFactor [Vietnam]
   ObjectTemplate.SetKitActiveName
   ObjectTemplate.SetKitIcon
   ObjectTemplate.SetKitName
   ObjectTemplate.SetKitSpeedFactor [Vietnam]
   ObjectTemplate.SetKitTeam
   ObjectTemplate.SetLensFlareCount
   ObjectTemplate.SetLiePointUpDownAngle
   ObjectTemplate.SetLodValue
   ObjectTemplate.SetMaxRollGroup [Vietnam]
   ObjectTemplate.SetMaxRotation
   ObjectTemplate.SetMaxSpeed
   ObjectTemplate.SetMinDev
   ObjectTemplate.SetMinimapIcon
   ObjectTemplate.SetMinimapIconSize
   ObjectTemplate.SetMinRotation
   ObjectTemplate.SetMiscDev
   ObjectTemplate.SetName
   ObjectTemplate.SetNetworkableInfo
   ObjectTemplate.SetNoPropellerEffectAtSpeed
   ObjectTemplate.SetNumberOfGears
   ObjectTemplate.SetNumberOfWeaponIcons
   ObjectTemplate.SetObjectiveDelay
   ObjectTemplate.SetObjectTemplate
   ObjectTemplate.SetParachuteDrag
   ObjectTemplate.SetParachuteSpeed
   ObjectTemplate.SetPassiveAcceleration
   ObjectTemplate.SetPcoId
   ObjectTemplate.SetPitchOffset
   ObjectTemplate.SetPivotPosition
   ObjectTemplate.SetPointUpDownAngle
   ObjectTemplate.SetPoseCameraPos
   ObjectTemplate.SetPosition
   ObjectTemplate.SetPositionOffset
   ObjectTemplate.SetPrimaryAmmoBar
   ObjectTemplate.SetPrimaryAmmoIcon
   ObjectTemplate.SetRadioLanguage
   ObjectTemplate.SetRandomGeometries
   ObjectTemplate.SetRecoilForceLeftRight
   ObjectTemplate.SetRecoilForceUp
   ObjectTemplate.SetRegulateToLift
   ObjectTemplate.SetRotation
   ObjectTemplate.SetScopeIcon
   ObjectTemplate.SetSecondaryAmmoBar
   ObjectTemplate.SetSecondaryAmmoIcon
   ObjectTemplate.SetSightIcon
   ObjectTemplate.SetSinkingSpeedMod
   ObjectTemplate.SetSkeletonCollisionBone
   ObjectTemplate.SetSniperSight
   ObjectTemplate.SetSoldierCrouchIcon
   ObjectTemplate.SetSoldierExitLocation
   ObjectTemplate.SetSoldierProneIcon
   ObjectTemplate.SetSoldierStandingIcon
   ObjectTemplate.SetSpawnAsParaTroper
   ObjectTemplate.SetSpawnDelay
   ObjectTemplate.SetSpawnId
   ObjectTemplate.SetSpawnPositionOffset
   ObjectTemplate.SetSpawnPreventionDelay
   ObjectTemplate.SetSpawnRotation
   ObjectTemplate.SetSpeedDev
   ObjectTemplate.SetStartOnEffects
   ObjectTemplate.SetStrength
   ObjectTemplate.SetSubmarineHudDepthModifier
   ObjectTemplate.SetSubmarineHudDirModifier
   ObjectTemplate.SetTakeable
   ObjectTemplate.SetTargetName
   ObjectTemplate.SetTeam
   ObjectTemplate.SetTeamFlagIcon
   ObjectTemplate.SetTeamGeometry
   ObjectTemplate.SetTicketIcon
   ObjectTemplate.SetToolTipType
   ObjectTemplate.SetTorque
   ObjectTemplate.SetTracerTemplate
   ObjectTemplate.SetTrigger
   ObjectTemplate.SetTurnDev
   ObjectTemplate.SetTurnLeftRightAngle
   ObjectTemplate.SetType
   ObjectTemplate.SetUseRollingSpawnGroup [Vietnam]
   ObjectTemplate.SetVehicleCategory
   ObjectTemplate.SetVehicleIcon
   ObjectTemplate.SetVehicleIconPos
   ObjectTemplate.SetVehicleType
   ObjectTemplate.SetVisibilityAngleDeg
   ObjectTemplate.SetVisibleDummyProjectileTemplate
   ObjectTemplate.SetWeaponLink
   ObjectTemplate.SetWingLift
   ObjectTemplate.SetWingToRegulatorRatio
   ObjectTemplate.ShowInFirstPerson
   ObjectTemplate.ShowInThirdPerson
   ObjectTemplate.ShowPCOCockpit [Vietnam]
   ObjectTemplate.SightIcon [Vietnam]
   ObjectTemplate.SinkInToLandAfterDeathSpeed
   ObjectTemplate.Size
   ObjectTemplate.SizeModifier
   ObjectTemplate.SizeOverTime
   ObjectTemplate.SkinIcon [Vietnam]
   ObjectTemplate.SniperSight [Vietnam]
   ObjectTemplate.SoldierCameraPosition
   ObjectTemplate.SoldierCrouchIcon [Vietnam]
   ObjectTemplate.SoldierProneIcon [Vietnam]
   ObjectTemplate.SoldierStandingIcon [Vietnam]
   ObjectTemplate.SoldierZoomFov
   ObjectTemplate.SoldierZoomPosition
   ObjectTemplate.SonarPos
   ObjectTemplate.SongId [Vietnam]
   ObjectTemplate.SpawnDelay
   ObjectTemplate.SpawnDelayAtStart
   ObjectTemplate.SpawnDistance [Vietnam]
   ObjectTemplate.SpawnGroupId
   ObjectTemplate.SpawnOffset
   ObjectTemplate.SpeedMod
   ObjectTemplate.Speedmodifier [Vietnam]
   ObjectTemplate.SpinWhenNoEngineInAir [Vietnam]
   ObjectTemplate.SpreadAngle [Vietnam]
   ObjectTemplate.SrcBlendMode
   ObjectTemplate.StartAtCreation
   ObjectTemplate.StartEffectTemplate
   ObjectTemplate.StartingMag [Vietnam]
   ObjectTemplate.Startoneffects
   ObjectTemplate.StartProbability
   ObjectTemplate.StartRotation
   ObjectTemplate.StayAsDestroyed
   ObjectTemplate.StickModifier [Vietnam]
   ObjectTemplate.Stiffness [Vietnam]
   ObjectTemplate.StopAtEndEffect
   ObjectTemplate.Strength [Vietnam]
   ObjectTemplate.SubDev
   ObjectTemplate.SubDevCrouching
   ObjectTemplate.SubDevLying
   ObjectTemplate.SubDevStanding
   ObjectTemplate.SubmarineData
   ObjectTemplate.SubPos
   ObjectTemplate.Team
   ObjectTemplate.TeamFlagIcon [Vietnam]
   ObjectTemplate.TeamOnVehicle
   ObjectTemplate.Template
   ObjectTemplate.Texture
   ObjectTemplate.TicketIcon [Vietnam]
   ObjectTemplate.TimeDelayOnOverHeat
   ObjectTemplate.TimeLimit
   ObjectTemplate.TimeOnEndEffect
   ObjectTemplate.TimeToGetControl
   ObjectTemplate.TimeToLive
   ObjectTemplate.TimeToLiveAfterDeath
   ObjectTemplate.TimeToLoseControl
   ObjectTemplate.TimeToNotAllowChange
   ObjectTemplate.TimeToReSpawn
   ObjectTemplate.TimeToSpawn [Vietnam]
   ObjectTemplate.TimeToStartFadeAfterDeath
   ObjectTemplate.ToggleMouseLook
   ObjectTemplate.ToolTipType [Vietnam]
   ObjectTemplate.Torque [Vietnam]
   ObjectTemplate.TowingInfluence [Vietnam]
   ObjectTemplate.TracerScaler
   ObjectTemplate.TriggerRadius
   ObjectTemplate.TTLDependentOnHeat [Vietnam]
   ObjectTemplate.TurnsInMovingDirection
   ObjectTemplate.Type [Vietnam]
   ObjectTemplate.UnableToChangeTeam
   ObjectTemplate.UnZoomBetweenFireTime
   ObjectTemplate.UpAngleMultiplier
   ObjectTemplate.UseAsBone
   ObjectTemplate.UseButtonRadius
   ObjectTemplate.UseMipMap
   ObjectTemplate.UseMipMaps
   ObjectTemplate.UseMMOnEndEffect
   ObjectTemplate.UseScope
   ObjectTemplate.UseSkeletonPartAsMain
   ObjectTemplate.UseUVRotation
   ObjectTemplate.VehicleCameraShake
   ObjectTemplate.VehicleCategory [Vietnam]
   ObjectTemplate.VehicleFov
   ObjectTemplate.VehicleIcon [Vietnam]
   ObjectTemplate.VehicleIconPos [Vietnam]
   ObjectTemplate.VehicleType [Vietnam]
   ObjectTemplate.Velocity
   ObjectTemplate.VelocityDependentOnHeat
   ObjectTemplate.VisibleBarrelTemplate
   ObjectTemplate.VisibleDummyProjectileTemplate
   ObjectTemplate.WaterDamageDelay
   ObjectTemplate.WingLift [Vietnam]
   ObjectTemplate.WireLinkTemplateName [Vietnam]
   ObjectTemplate.WorkOnSoldiers
   ObjectTemplate.WorkOnVehicles
   ObjectTemplate.XYSizeRatio
   ObjectTemplate.XYSizeRatioOverTime
   ObjectTemplate.XZAccelerationFactor [Vietnam]
   ObjectTemplate.YModOnExplosion
   ObjectTemplate.YOffset [Vietnam]
   ObjectTemplate.ZOffset [Vietnam]
   ObjectTemplate.ZoomedAccelFactor [Vietnam]
   ObjectTemplate.ZoomedSpeedFactor [Vietnam]
   ObjectTemplate.ZoomFov
*)
  end;

  TBfObject = class(TObject)

  (*
   Object.Absoluteposition
   Object.Active
   Object.Create
   Object.Geometry.color
   Object.Geometry.scale
   Object.IsSaveable
   Object.Name
   Object.Rotation
   Object.SetName
   Object.SetOSId
   Object.SetTeam
   *)
    AbsolutePosition : TBfAbsolutePosition;
    constructor Create(Name:string; Template:TBfObjectTemplate);

  end;


implementation

{ TBfObject }



{ TBfObject }

constructor TBfObject.Create(Name:string; Template:TBfObjectTemplate);
begin

end;

end.
