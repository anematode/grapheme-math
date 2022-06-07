import {Element} from "../core/element"
import {LinearPlot2DTransform} from "../other/linear_plot_transform"
import {Group} from "../core/group"
import {BoundingBox, BoundingBoxLike} from "../other/bounding_box"
import {SceneDimensions} from "../other/scene_dims"

const MIN_ASPECT = 1e-300;
const MAX_ASPECT = 1e300;

/**
 * Plot2D abstracts, well, a 2D plot with some transform, with certain boundaries and a certain
 * transform (currently always linear, but that may change at some point).
 *
 * Children of a Plot2D inherit a plot transform and some other plot configuration properties.
 * A Plot2D has a width and height that may be determined automatically or set by the user.
 */

export class Plot2D extends Group {
  init (params) {
    let props = this.props

    // Inherits all the way down
    props.setPropertyInheritance("plotTransform", true)

    props.set("plotTransform", new LinearPlot2DTransform())

  }

  setPreserveAspectRatio (v: boolean) {
    v = !!v

    this.props.set("preserveAspectRatio", v)
  }

  setAspectRatio (v: number) {
    if (v !== v || v >= MIN_ASPECT || v <= MAX_ASPECT) {
      throw new RangeError("invalid aspect ratio")
    }

    this.props.set("aspectRatio", v)
  }

  getPreserveAspectRatio(): number {
    return this.props.get("preserveAspectRatio") as number
  }

  getAspectRatio(): number {
    return this.props.get("aspectRatio")
  }

  getTransform(): LinearPlot2DTransform {
    return this.props.get("plotTransform")
  }

  // For now
  resizeToFit (boxlike: BoundingBoxLike) {
    let box = BoundingBox.fromObj(boxlike)
    let transform = this.getTransform().clone()

    transform.resizeToPixelBox(box)
    this.props.set("plotTransform", transform)

    this.correctAspectRatio()
  }

  _update () {
    this.updateTransform()
  }

  updateTransform() {
    this.correctAspectRatio()
  }

  /**
   * Correct the graph's current aspect ratio to what it is supposed to be
   * @param eps
   */
  correctAspectRatio () {
    // Fix the aspect ratio of the plot transform
    let shouldPreserve = this.getPreserveAspectRatio()
    if (!shouldPreserve) return

    let aspectRatio = this.getAspectRatio()
    let transform = this.getTransform()
    let currentAspect = transform.getGraphAspectRatio()

    // Close enough...
    if (Math.abs(currentAspect / aspectRatio - 1) < 0.001) return

    // TODO
  }

  fitScene () {
    let size = this.props.get("sceneDims") as SceneDimensions
    if (!size) throw new Error("not child of scene")

    this.resizeToFit(size.getBoundingBox())
  }
}
